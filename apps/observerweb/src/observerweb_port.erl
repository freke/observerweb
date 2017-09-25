%%%-------------------------------------------------------------------
%%% @author David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-24
%%%-------------------------------------------------------------------
-module(observerweb_port).
-author("davabe@hotmail.com").

%% API
-export([port_info/1, port_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
port_info(Node) ->
  lists:filtermap(
    fun(Port) ->
      case port_info(Node, Port) of
        undefind -> false;
        P -> {true, P}
      end
    end
    , observerweb:try_rpc(Node, erlang, ports, [])).

port_info(Node, Port) ->
  case observerweb:try_rpc(Node, erlang, port_info, [Port]) of
    undefined ->
      undefind;
    PortInfo ->
      maps:map(
        fun
          (output,V) -> V;
          (input,V) -> V;
          (id,V) -> V;
          (links,V) ->
            lists:map( fun(P) -> list_to_binary(observerweb_lib:to_str(P)) end, V );
          (_,V) ->
            list_to_binary(observerweb_lib:to_str(V))
        end
        , maps:from_list(PortInfo)
        )
  end.
