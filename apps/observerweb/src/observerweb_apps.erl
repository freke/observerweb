%%%-------------------------------------------------------------------
%%% @author David AAberg
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-24
%%%-------------------------------------------------------------------
-module(observerweb_apps).
-author("davabe@hotmail.com").

%% API
-export([apps/1, apps/2]).

apps(Node) ->
  case observerweb:try_rpc(Node, application, which_applications, []) of
    undefined -> undefind;
    Apps -> lists:filtermap(
      fun({Name, Descr, Vsn}) ->
        case get_master_pid(Node, Name) of
          Pid when is_pid(Pid) ->
            {true, #{name=>Name
            , descr=>list_to_binary(Descr)
            , vsn=>list_to_binary(Vsn)
            }};
          _ -> false
        end
      end, Apps)
  end.

apps(Node, App) ->
  Pid = get_master_pid(Node, App),
  #{pid=>list_to_binary(observerweb_lib:to_str(Pid))
  , children => structure_pid(Node, get_pid_info(Node, Pid))
  , name => list_to_binary(observerweb_lib:to_str(get_name(Node, Pid)))
  , monitors => lists:map(
      fun({T,P}) ->
        #{type => list_to_binary(observerweb_lib:to_str(T))
        , pid => list_to_binary(observerweb_lib:to_str(P))
        }
      end, get_monitors(Node, Pid))
  }.

get_master_pid(Node, App) ->
  observerweb:try_rpc(Node, application_controller, get_master, [App]).

get_pid_info(Node, Pid) ->
  observerweb:try_rpc(Node, application_master, get_child, [Pid]).

get_name(Node, Pid) ->
  case observerweb:try_rpc(Node, erlang, process_info, [Pid, registered_name]) of
    {registered_name, RegisteredName} -> RegisteredName;
    _ -> Pid
  end.

get_dictionary(Node, Pid) ->
  observerweb:try_rpc(Node, erlang, process_info, [Pid, dictionary]).

get_links(Node, Pid) ->
  observerweb:try_rpc(Node, erlang, process_info, [Pid, links]).

get_children(Node, Pid) ->
  observerweb:try_rpc(Node, supervisor, which_children, [Pid]).

get_monitors(Node, Pid) ->
  case observerweb:try_rpc(Node, erlang, process_info, [Pid, monitors]) of
    {monitors, Monitors} -> Monitors;
    _ -> []
  end.

structure_pid(Node, {Pid, Name}) ->
  Child = structure_pid(Node, {Name, Pid, supervisor, []}),

  {dictionary, Dictionary} = get_dictionary(Node, Pid),

  case proplists:get_value('$ancestors', Dictionary, []) of
    [Pid] ->
      #{ pid=> list_to_binary(observerweb_lib:to_str(Pid))
      , children => Child
      , name => list_to_binary(observerweb_lib:to_str(get_name(Node, Pid)))
      , monitors => lists:map(
          fun({T,P}) ->
            #{type => list_to_binary(observerweb_lib:to_str(T))
            , pid => list_to_binary(observerweb_lib:to_str(P))
            }
          end, get_monitors(Node, Pid))
      };
    _ ->
      Child
  end;

structure_pid(_Node, {_, undefined, _, _}) -> [];

structure_pid(Node, {_, Pid, supervisor, _}) ->
  {links, Links} = get_links(Node, Pid),

  Children =
    case length(Links) of
      [_] ->
        [];
      _ ->
        C = get_children(Node, Pid),
        C1 = C ++ lists:filter(fun is_port/1, Links),
        lists:map(fun(L) -> structure_pid(Node,L) end, C1)
    end,

  #{pid => list_to_binary(observerweb_lib:to_str(Pid))
  , children => Children
  , name => list_to_binary(observerweb_lib:to_str(get_name(Node, Pid)))
  , monitors => lists:map(
      fun({T,P}) ->
        #{type => list_to_binary(observerweb_lib:to_str(T))
        , pid => list_to_binary(observerweb_lib:to_str(P))
        }
      end, get_monitors(Node, Pid))
  };

structure_pid(Node, {_, Pid, worker, _}) ->
  #{pid => list_to_binary(observerweb_lib:to_str(Pid))
  , children => []
  , name => list_to_binary(observerweb_lib:to_str(get_name(Node, Pid)))
  , monitors => lists:map(
      fun({T,P}) ->
        #{type => list_to_binary(observerweb_lib:to_str(T))
        , pid => list_to_binary(observerweb_lib:to_str(P))
        }
      end, get_monitors(Node, Pid))
  };

structure_pid(_Node, Port) when is_port(Port) ->
  #{pid => port
  , children => []
  , name => port
  , monitors => []
  }.
