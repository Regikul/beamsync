-module(beamsync_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, beamsync).
-define(DEPS, [compile]).

-record(mod, {
    name :: module(),
    abs_path :: file:filename(),
    content :: binary(),
    vsn :: [non_neg_integer()]
}).

-record(app, {
    name :: binary(),
    beams :: [#mod{}] | [file:filename_all()]
}).

-define(HELPER, beamsync_helper_with_unique_name).
-define(COL_LEN, 30).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 beamsync"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    DistName = dist_name(State),
    BeamSyncConfig = beamsync_config(),
    Nodes = proplists:get_value(nodes, BeamSyncConfig),
    BeamSyncName = find_names_cookie(BeamSyncConfig),
    ExcludeApps = get_excluded(apps, BeamSyncConfig),
    ExcludeModules = get_excluded(modules, BeamSyncConfig),
    {Long, Short, Cookie} = merge_names(DistName, BeamSyncName),
    rebar_dist_utils:either(Long, Short, Cookie),
    Apps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    AllApps = lists:map(
        fun read_all_modules/1,
        lists:map(fun describe_apps/1, Apps ++ Deps)
    ),
    AppNames = lists:map(fun (#app{name = A}) -> A end, AllApps),
    rebar_api:info("going to sync apps: ~p", [AppNames]),
    rebar_api:debug("node: ~p", [node()]),
    rebar_api:debug("remote nodes: ~p", [Nodes]),
    rebar_api:debug("cookie: ~p", [erlang:get_cookie()]),
    SyncApps = exclude(ExcludeApps, ExcludeModules, AllApps),
    Updated = sync_beams(SyncApps, Nodes),
    print_errors(
        lists:zip(Nodes, Updated)
    ),
    {ok, State}.

print_errors(Reports) ->
    lists:map(fun lookup_errors/1, Reports).

lookup_errors({Node, NodeReport}) ->
    case lists:filter(fun just_errors/1, NodeReport) of
        [] -> ok;
        [_ | _] = Errors -> lists:foreach(do_print_errors(Node), Errors)
    end.

do_print_errors(Node) ->
    rebar_api:error("failed to update on node ~s:", [Node]),
    fun ({{error, Reason}, App}) ->
        rebar_api:error("~s: ~p", [App, Reason])
    end.

just_errors({ok, _}) -> false;
just_errors({{error, _}, _}) -> true.

-spec exclude([atom()], [atom()], [#app{}]) -> [#app{}].
exclude(Apps, Modules, SyncApps) ->
    lists:filter(
        outside_of(Apps),
        lists:map(exclude_modules(Modules), SyncApps)
    ).

-spec outside_of([#app{}] | [#mod{}]) -> fun ((#app{} | #mod{}) -> boolean()).
outside_of(ExcludeList) ->
    fun
        (#app{name = Name}) ->
            not lists:member(binary_to_atom(Name, utf8), ExcludeList);
        (#mod{name = Name}) ->
            not lists:member(Name, ExcludeList)
    end.

-spec exclude_modules([atom()]) -> fun((#app{}) -> #app{}).
exclude_modules(Modules) ->
    fun (#app{beams = Beams} = App) ->
        App#app{
            beams = lists:filter(outside_of(Modules), Beams)
        }
    end.

-spec describe_apps(rebar_app_info:t()) -> #app{}.
describe_apps(App) ->
    Name = rebar_app_info:name(App),
    EbinDir = rebar_app_info:ebin_dir(App),
    Beams = filelib:wildcard(
        filename:join([EbinDir, "*.beam"])
    ),
    #app{
        name = Name,
        beams = Beams
    }.

-spec read_all_modules(#app{}) -> #app{}.
read_all_modules(#app{} = App) ->
    #app{beams = AppFiles} = App,
    File2Module = fun (AbsPath) ->
        BinPath = list_to_binary(AbsPath),
        Basename = filename:basename(BinPath),
        {DotPos, _} = lists:last(binary:matches(Basename, <<".">>)),
        ModuleName = binary:part(Basename, {0, DotPos}),
        {ok, Content} = file:read_file(BinPath),
        {ok, {_, Vsn}} = beam_lib:version(Content),
        #mod{
            name = list_to_atom(
                binary_to_list(ModuleName)
            ),
            abs_path = AbsPath,
            content = Content,
            vsn = Vsn
        }
    end,
    App#app{
         beams = lists:map(File2Module, AppFiles)
    }.

-spec sync_beams([#app{}], [node()]) -> list().
sync_beams(Apps, Nodes) ->
    pmap(sync_node(Apps), Nodes).

-spec sync_node([#app{}]) -> fun((node()) -> list()).
sync_node(Apps) ->
    fun(Node) ->
        net_kernel:hidden_connect_node(Node),
        with_loaded_helper(Node, fun() ->
            rebar_api:info("updating node ~p", [Node]),
            pmap(update_app_on_node(Node), Apps)
        end)
    end.

with_loaded_helper(Node, Fun) ->
    ?HELPER:migrate(Node),
    Ret = Fun(),
    ?HELPER:unload(Node),
    Ret.

-spec update_app_on_node(node()) -> fun((#app{}) -> ok).
update_app_on_node(Node) ->
    Prepare = fun (#mod{vsn = Vsn, name = Module}) ->
        {Module, Vsn}
    end,
    fun(App) ->
        ModulesList = lists:map(Prepare, App#app.beams),
        UpdateRequired = rpc:call(Node, ?HELPER, check, [ModulesList]),
        UpdateList = make_update_list(App#app.beams, UpdateRequired),
        do_update(Node, App#app{beams = UpdateList})
    end.

make_update_list(Mods, UpdateRequired) ->
    F = fun (#mod{name = Name} = Module, Acc) ->
        case proplists:is_defined(Name, UpdateRequired) of
            true -> [Module | Acc];
            false -> Acc
        end
    end,
    lists:foldl(F, [], Mods).

-spec do_update(node(), #app{}) -> term().
do_update(_Node, #app{beams = []} = App) ->
    {ok, App#app.name};
do_update(Node, App) ->
    Prepare = fun (#mod{} = M, Acc) ->
        #mod{
            name = Name,
            content = Content,
            abs_path = Path
        } = M,
        rpc:call(Node, code, purge, [Name]),
        [{Name, Path, Content} | Acc]
    end,
    CodeMods = lists:foldl(Prepare, [], App#app.beams),
    Modules = lists:map(fun (#mod{name = N}) -> N end, App#app.beams),
    rebar_api:console("syncing modules for ~s on ~s: ~p", [App#app.name, Node, Modules]),
    Res = rpc:call(Node, code, atomic_load, [CodeMods]),
    {Res, App#app.name}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

dist_name(State) ->
    {Long, Short, Opts} = rebar_dist_utils:find_options(State),
    [BeamSyncLong, BeamSyncShort] = lists:map(fun maybe_rewrite_name/1, [Long, Short]),
    {BeamSyncLong, BeamSyncShort, Opts}.

beamsync_config() ->
    RebarConfig = rebar_config:consult_root(),
    proplists:get_value(beamsync, RebarConfig, []).

get_excluded(Entity, Config) when Entity =:= apps orelse Entity =:= modules ->
    case proplists:get_value(excluded, Config, []) of
        [] -> [];
        Excluded -> proplists:get_value(Entity, Excluded, [])
    end.

find_names_cookie(Props) ->
    Mode = proplists:get_value(mode, Props),
    Cookie = case lists:keyfind(setcookie, 1, Props) of
                 false -> [];
                 Tuple when is_tuple(Tuple) -> [Tuple]
             end,
    case Mode of
        undefined -> {undefined, undefined, Cookie};
        longname -> {maybe_rewrite_name(rewrite), undefined, Cookie};
        shortname ->{undefined, maybe_rewrite_name(rewrite), Cookie}
    end.

merge_names({_,     _,      _},       {BLong, _,      BCookie}) when BLong =/= undefined  -> {BLong,     undefined, BCookie};
merge_names({_,     _,      _},       {_,     BShort, BCookie}) when BShort =/= undefined -> {undefined, BShort,    BCookie};
merge_names({DLong, _,      DCookie}, {_,     _,      _})       when DLong =/= undefined  -> {DLong,     undefined, DCookie};
merge_names({_,     DShort, DCookie}, {_,     _,      _})       when DShort =/= undefined -> {undefined, DShort,    DCookie};
merge_names({_,     _,      _},       {_,     _,      _})                                 -> {undefined, undefined, []}.

maybe_rewrite_name(undefined) ->
    undefined;
maybe_rewrite_name(Atom) when is_atom(Atom) ->
    list_to_atom(
        "beamsync_" ++ integer_to_list(
            binary:decode_unsigned(
                crypto:strong_rand_bytes(2)
            )
        )
    ).

pmap(F, List) ->
    par(F, List, map).

%%pforeach(F, List) ->
%%    par(F, List, foreach).

par(F, List, Impl) when Impl =:= map orelse Impl =:= foreach ->
    Self = self(),
    SpawnFun = fun (X) ->
        Ref = erlang:make_ref(),
        erlang:spawn(fun() ->
            Self ! {'$pmap', Ref, F(X)}
                     end),
        Ref
    end,
    RecvFun = fun (Ref) ->
        receive
            {'$pmap', Ref, V} -> V
        end
    end,
    lists:Impl(RecvFun,
        lists:map(SpawnFun, List)
    ).
