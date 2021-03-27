-module(beamsync_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, beamsync).
-define(DEPS, [compile]).

-record(beamsync, {
    nodes = [] :: [atom()],
    mode :: shortname | longname
}).

-record(mod, {
    name :: module(),
    abs_path :: file:filename(),
    content :: binary(),
    vsn :: binary()
}).

-record(app, {
    name :: binary(),
    beams :: list()
}).

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
%%    net_kernel:hidden_connect_node(Nodename),
    Nodes = get_nodes(),
    DistName = dist_name(State),
    BeamSyncName = beamsync_name(),
    {Long, Short, Cookie} = merge_names(DistName, BeamSyncName),
    rebar_dist_utils:either(Long, Short, Cookie),
    Apps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    SyncApps = lists:map(
        fun read_all_modules/1,
        lists:map(fun describe_apps/1, Apps ++ Deps)
    ),
    AppNames = lists:map(fun (#app{name = A}) -> A end, SyncApps),
    rebar_api:info("going to sync apps: ~p", [AppNames]),
    rebar_api:debug("node: ~p", [node()]),
    rebar_api:debug("remote nodes: ~p", [Nodes]),
    rebar_api:debug("cookie: ~p", [erlang:get_cookie()]),
    sync_beams(SyncApps, Nodes),
    {ok, State}.

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
        {ok, {_, [Vsn]}} = beam_lib:version(Content),
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

-spec sync_beams([#app{}], [node()]) -> ok.
sync_beams(Apps, Nodes) ->
    lists:foreach(sync_node(Apps), Nodes).

-spec sync_node([#app{}]) -> fun((node()) -> ok).
sync_node(Apps) ->
    fun(Node) ->
        rebar_api:info("updating node ~p", [Node]),
        net_kernel:hidden_connect_node(Node),
        lists:foreach(update_app_on_node(Node), Apps)
    end.

-spec update_app_on_node(node()) -> fun((#app{}) -> ok).
update_app_on_node(Node) ->
    fun(App) ->
        UpdateRequired = lists:filter(is_outdated(Node), App#app.beams),
        do_update(Node, App#app{beams = UpdateRequired})
    end.

-spec is_outdated(node()) -> fun((#mod{}) -> boolean()).
is_outdated(Node) ->
    fun (#mod{} = Mod) ->
        #mod{
            vsn = OurVsn,
            name = Module
        } = Mod,
        TheirAttrs = rpc:call(Node, Module, module_info, [attributes]),
        [TheirVsn] = case TheirAttrs of
                         List when is_list(List) -> proplists:get_value(vsn, TheirAttrs);
                         _Else -> [not_found]
                     end,
        case TheirVsn =/= OurVsn of
            true when is_integer(TheirVsn) ->
                rebar_api:console("~p should be updated, versions differ", [Module]),
                true;
            _ ->
                false
        end
    end.

-spec do_update(node(), #app{}) -> term().
do_update(_Node, #app{beams = []}) ->
    ok;
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
    Res = rpc:call(Node, code, atomic_load, [CodeMods]),
    rebar_api:console("~s updated with result: ~p", [App#app.name, Res]),
    Res.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

dist_name(State) ->
    {Long, Short, Opts} = rebar_dist_utils:find_options(State),
    [BeamSyncLong, BeamSyncShort] = lists:map(fun maybe_rewrite_name/1, [Long, Short]),
    {BeamSyncLong, BeamSyncShort, Opts}.

beamsync_name() ->
    RebarConfig = rebar_config:consult_root(),
    case proplists:get_value(beamsync, RebarConfig) of
        undefined -> {undefined, undefined, undefined};
        [] -> {undefined, undefined, undefined};
        [_ | _] = List -> find_names_cookie(List)
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

get_nodes() ->
    RebarConfig = rebar_config:consult_root(),
    case proplists:get_value(beamsync, RebarConfig) of
        undefined -> [];
        [] -> [];
        [_ | _] = List -> proplists:get_value(nodes, List)
    end.

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