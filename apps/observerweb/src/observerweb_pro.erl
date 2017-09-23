%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_pro).
-author("bill@freecnpro.net").

%% API
-export([pro_info/1, pro_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
pro_info(Node) ->
  L = [ registered_name
      , priority
      , trap_exit
      , initial_call
      , reductions
      , current_function
      , message_queue_len
      , error_handler
      , group_leader
      , links
      , monitors
      , memory
      , total_heap_size
      , heap_size
      , stack_size
      , min_heap_size
      , garbage_collection
      , status
      , dictionary
      ],
    HFun =
      fun
        (max_heap_size, V) -> V;
        (_,V) -> list_to_binary(observerweb_lib:to_str(V))
      end,
    GFun =
      fun
        (garbage_collection,V) -> maps:map(HFun, maps:from_list(V));
        (links,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (monitors,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (ancestors,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (_,V) -> list_to_binary(observerweb_lib:to_str(V))
      end,
    lists:filtermap(
      fun(P) ->
        case observerweb:try_rpc(Node, erlang, process_info, [P,L]) of
          undefined -> false;
          [] -> false;
          Info ->
            Map = maps:from_list([ {pid,P} | Info ]),
            Dic = maps:get(dictionary, Map),
            Ans = proplists:get_value('$ancestors', Dic, []),
            { true
            , maps:map(GFun, maps:put(ancestors, Ans, Map))
            }
        end
      end
      , observerweb:try_rpc(Node, erlang, processes, [])).

pro_info(Node, Pid) ->
  L = [ registered_name
      , priority
      , trap_exit
      , initial_call
      , reductions
      , current_function
      , message_queue_len
      , error_handler
      , group_leader
      , links
      , monitors
      , memory
      , total_heap_size
      , heap_size
      , stack_size
      , min_heap_size
      , garbage_collection
      , status
      , dictionary
      ],
    HFun =
      fun
        (max_heap_size, V) -> V;
        (_,V) -> list_to_binary(observerweb_lib:to_str(V))
      end,
    GFun =
      fun
        (garbage_collection,V) -> maps:map(HFun, maps:from_list(V));
        (links,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (monitors,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (ancestors,V) -> lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
        (_,V) -> list_to_binary(observerweb_lib:to_str(V))
      end,

    case observerweb:try_rpc(Node, erlang, process_info, [Pid,L]) of
      undefined -> false;
      [] -> false;
      Info ->
        Map = maps:from_list([ {pid,Pid} | Info ]),
        Dic = maps:get(dictionary, Map),
        Ans = proplists:get_value('$ancestors', Dic, []),
        { true
        , maps:map(GFun, maps:put(ancestors, Ans, Map))
        }
    end.
