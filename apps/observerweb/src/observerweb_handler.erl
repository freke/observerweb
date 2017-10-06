%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_handler).
-author("bill@freecnpro.net").

-include("observerweb.hrl").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = process(Method, HasBody, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

process(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
process(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    {N1, Req3} = cowboy_req:cookie(<<"current_node">>, Req2, atom_to_binary(node(),latin1)),
    CurrentNode = binary_to_atom(N1,latin1),
    case proplists:get_value(<<"action">>, PostVals) of
        <<"get_sys">> ->
            Body = do_process(get_sys, CurrentNode),
            reply(200, Body, Req3);
        <<"get_perf">> ->
            Type = proplists:get_value(<<"type">>, PostVals),
            Body = do_process(get_perf, {CurrentNode, binary_to_atom(Type, latin1)}),
            reply(200, Body, Req3);
        <<"get_malloc">> ->
            Body = do_process(get_malloc, CurrentNode),
            reply(200, Body, Req3);
        <<"get_pro">> ->
            %Type = proplists:get_value(<<"type">>, PostVals),
            Body = do_process(get_pro, CurrentNode),
            reply(200, Body, Req3);
        <<"get_ports">> ->
            Body = do_process(get_ports, CurrentNode),
            reply(200, Body, Req3);
        <<"get_tables">> ->
            Table = proplists:get_value(<<"table">>, PostVals),
            Body = do_process(get_tables, {CurrentNode, Table}),
            reply(200, Body, Req3);
        <<"get_apps">> ->
            App = proplists:get_value(<<"app">>, PostVals),
            Body = do_process(get_apps, {CurrentNode, App}),
            reply(200, Body, Req3);
        <<"change_node">> ->
            Node = binary_to_atom(proplists:get_value(<<"node">>, PostVals), latin1),
            Result = case do_process(change_node, Node) of
                pang ->
                    jiffy:encode({[{node,Node},{connected,false}]});
                pong ->
                    %insert_Data(acc_node, Node),
                    observerweb_pro:change_node(Node),
                    jiffy:encode({[{node,Node},{connected,true}]});
                false ->
                    jiffy:encode({[{node,Node},{connected,false},{message, <<"Node invalid">>}]})
            end,
            reply(200, Result, Req3);
        <<"connect_node">> ->
            Node = proplists:get_value(<<"node">>, PostVals),
            Cookie = proplists:get_value(<<"cookie">>, PostVals),
            {Result,Req4} = case do_process(connect_node, {Node, Cookie}) of
              pang ->
                { jiffy:encode({[{node,Node},{connected,false},{message, <<"Connection failed">>}]})
                , Req3
                };
              pong ->
                { jiffy:encode({[{node,Node},{connected,true}]})
                , cowboy_req:set_resp_cookie(<<"current_node">>, Node, [], Req3)
                }
            end,
            reply(200, Result, Req4);
        <<"get_nodes">> ->
            Body = jiffy:encode({[{<<"nodes">>, get_bare_nodes()}]}),
            reply(200, Body, Req3);
        <<"del_node">> ->
            Node = proplists:get_value(<<"node">>, PostVals),
            del_node(Node),
            Req3
    end;
process(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Body, Req).

do_process(get_sys, Node) ->
    {Info, Stat} = observerweb_sys:sys_info(Node),
    [{_SysName, SysValue},{_CPUName, CPUValue}] = Info,
    [{_MemName, MemValue},{_StatName, StatValue}] = Stat,
    jiffy:encode({[{<<"system">>, wrap_info(info, SysValue)},
        {<<"cpu">>, wrap_info(info, CPUValue)},
        {<<"memory">>, wrap_info(info, MemValue)},
        {<<"statistics">>, wrap_info(info, StatValue)}]});

do_process(get_perf, {Node, Type}) ->
    Data0 = observerweb_perf:perf_info(Node, Type),
    case Type of
        scheduler ->
            Data = wrap_info(scheduler, Data0),
            jiffy:encode({[{<<"scheduler">>, Data}]});
        _ ->
            jiffy:encode({[{<<"time">>,iso8601:now()}|Data0]})
    end;

do_process(get_malloc, Node) ->
    Data = observerweb_alloc:memory_alloc_info(Node),
    jiffy:encode({[{<<"allocator">>, wrap_info(alloc, Data)}]});

do_process(get_pro, Node) ->
  Data = observerweb_pro:pro_info(Node),
  jiffy:encode({[{<<"process_table">>, Data}]});

do_process(get_ports, Node) ->
  Data = observerweb_port:port_info(Node),
  jiffy:encode({[{<<"port_table">>, Data}]});

do_process(get_tables, {Node,undefined}) ->
  Data = observerweb_table:table_info(Node),
  jiffy:encode({[{<<"ets_table">>, Data}]});
do_process(get_tables, {Node,Table}) ->
  T = erlang:binary_to_existing_atom(Table, latin1),
  Data = observerweb_table:table_data(Node,T),
  jiffy:encode({[{<<"ets_table_data">>, Data}]});

do_process(get_apps, {Node,undefined}) ->
  Data = observerweb_apps:apps(Node),
  jiffy:encode(#{apps => Data});
do_process(get_apps, {Node,App}) ->
  A = erlang:binary_to_existing_atom(App, latin1),
  Data = observerweb_apps:apps(Node,A),
  jiffy:encode(#{app => Data});

do_process(change_node, Node) ->
    case lists:member(Node, get_bare_nodes()) of
        true ->
            net_adm:ping(Node);
        false ->
            false
    end;
do_process(connect_node, {Value1, Value2}) ->
    try
        Node = binary_to_atom(Value1, latin1),
        Cookie = binary_to_atom(Value2, latin1),
        io:format("Node: ~p~nCookie:~p~n", [Node, Cookie]),
        erlang:set_cookie(node(), Cookie),
        net_adm:ping(Node)
    catch _:_ ->
        pang
    end.

del_node(Node) ->
    erlang:disconnect_node(Node).

wrap_info(Type, Info) ->
    {[{<<"time">>,iso8601:now()}, {data,  wrap_info2(Type, Info, [])}]}.

wrap_info2(alloc, [], Data) -> lists:reverse(Data);
wrap_info2(alloc, [{Name, BS, CS}|Alloc], Data) ->
    wrap_info2( alloc
              , Alloc
              , [ { [ {<<"name">>, Name}
                    , {<<"bs">>, (BS div 1024)}
                    , {<<"cs">>, (CS div 1024)}
                    ]
                  }
                | Data
                ]
              );

wrap_info2(scheduler, [], Data) -> lists:reverse(Data);
wrap_info2(scheduler, [{SchedulerId, ActiveTime, TotalTime}|Scheduler], Data) ->
    wrap_info2( scheduler
              , Scheduler
              , [ { [ {<<"schedulerid">>, SchedulerId}
                    , {<<"activetime">>, ActiveTime}
                    , {<<"totaltime">>, TotalTime}
                    ]
                  }
                | Data
                ]
              );

wrap_info2(info, [], Data) -> lists:reverse(Data);
wrap_info2(info, [{Name, Value}|Stat], Data) ->
    wrap_info2( info
              , Stat
              , [ { [ {<<"name">>, list_to_binary(Name)}
                    , {<<"value">>, list_to_binary(observerweb_lib:to_str(Value))}
                    ]
                  }
                | Data
                ]
              ).

get_bare_nodes() ->
  [node() | nodes(connected)].
