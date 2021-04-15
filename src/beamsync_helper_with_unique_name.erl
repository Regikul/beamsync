-module(beamsync_helper_with_unique_name).

%% API
-export([
    migrate/1,
    unload/1,
    check/1
]).

migrate(Node) ->
    case node() of
        Node -> ok;
        _ ->
            ModPath = code:which(?MODULE),
            {ok, Content} = file:read_file(ModPath),
            rpc:call(Node, code, purge, [?MODULE]),
            {module, _} = rpc:call(Node, code, load_binary, [?MODULE, ?MODULE_STRING ++ ".erl", Content])
    end.

unload(Node) ->
    case node() of
        Node -> ok;
        _ ->
            rpc:call(Node, code, delete, [?MODULE]),
            rpc:call(Node, code, soft_purge, [?MODULE])
    end.

-type version() :: [non_neg_integer()].
-spec check([{module(), version()}]) -> [module()].
check(Modules) ->
    Outdated = fun ({Mod, Vsn}) ->
        LoadedVsn = loaded_vsn(Mod),
        LoadedVsn =/= undefined andalso Vsn =/= LoadedVsn
    end,
    lists:filter(Outdated, Modules).

-spec loaded_vsn(module()) -> undefined | version().
loaded_vsn(Module) ->
    try Module:module_info(attributes) of
        Attrs when is_list(Attrs) -> proplists:get_value(vsn, Attrs);
        _Else -> undefined
    catch
        error:undef -> undefined
    end.
