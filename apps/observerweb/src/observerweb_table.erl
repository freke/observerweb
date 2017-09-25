%%%-------------------------------------------------------------------
%%% @author David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-24
%%%-------------------------------------------------------------------
-module(observerweb_table).
-author("davabe@hotmail.com").

%% API
-export([table_info/1, table_info/2, table_data/2]).

%%%===================================================================
%%% API
%%%===================================================================
table_info(Node) ->
  lists:filtermap(
    fun(Table) ->
      case table_info(Node, Table) of
        undefind -> false;
        T -> {true, T}
      end
    end
    , observerweb:try_rpc(Node, ets, all, [])).

table_info(Node, Table) ->
  case observerweb:try_rpc(Node, ets, info, [Table]) of
    undefined ->
      undefind;
    TableInfo ->
      maps:map(
        fun
          (compressed,V) -> V;
          (size,V) -> V;
          (memory,V) -> V;
          (keypos,V) -> V;
          (named_table,V) -> V;
          (write_concurrency,V) -> V;
          (read_concurrency,V) -> V;
          (_,V) ->
            list_to_binary(observerweb_lib:to_str(V))
        end
        , maps:from_list(TableInfo)
        )
  end.

table_data(Node, Tab) ->
  D = observerweb:try_rpc(Node, ets, tab2list, [Tab]),
  lists:map(fun(E) -> list_to_binary(io_lib:format("~tp", [E])) end, D).
