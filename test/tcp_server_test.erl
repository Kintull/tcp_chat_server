-module(tcp_server_test).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-record(user_id, {nickname, socket}).
-record(message, {timestamp, user_id = #user_id{}, message_data}).

-export([start_client/0]).

%% ====================================================================
%% TESTS DESCRIPTIONS
%% ====================================================================

join_quit_client_test_() ->
{"Join the chat and try to send two messages, then quit",
 ?setup(fun join_chat_send_message_normal/1)}.

user_list_test_() ->
    [{"Add and delete user",
     ?setup(fun add_delete_user_normal/1)},
     {"Add users with name collision",
     ?setup(fun add_user_name_collision/1)},
     {"Add user and update nickname",
     ?setup(fun add_user_update_nickname/1)}].

history_test_() ->
    {"Send several messages and check stored messages",
     ?setup(fun store_history_test/1)}.

%% ====================================================================
%% SETUP FUNCTIONS
%% ====================================================================

start() ->
    application:start(main),
    ok.

stop(_) ->
    application:stop(main),
    ok.

%% ====================================================================
%% TESTS
%% ====================================================================

%test_user_table({_ServerPid, _ClientPid, _ListenSocket})>
%case ase gen_server:call(?server_mgr, {check_nickname, Sock"", "User1"})

add_delete_user_normal(_) ->
    Nick1 = "User1",
    Nick2 = "User2",
    ClientPid1 = spawn_join_client(Nick1),

    timer:sleep(500),

    ClientPid2 = spawn_join_client(Nick2),
    timer:sleep(500),

    [#user_id{nickname = Nick1}, #user_id{nickname = Nick2}] = ets:tab2list(user_list),

    ClientPid1!stop,
    ClientPid2!stop,

    timer:sleep(500),
    ?_assertEqual(ets:tab2list(user_list), []).

add_user_name_collision(_) ->
    Nick1 = "User1",
    Nick2 = "User2",
    Nick3 = "User3",
    ClientPid1 = spawn_join_client(Nick1),

    timer:sleep(500),

    ClientPid2 = spawn_join_client(Nick2),
    timer:sleep(500),

    ClientPid3 = spawn_join_client(Nick2),
    timer:sleep(500),

    [#user_id{nickname = Nick1},
     #user_id{nickname = Nick2}] = ets:tab2list(user_list),

    ClientPid4 = spawn_join_client(Nick3),
    timer:sleep(500),

    [#user_id{nickname = Nick1},
     #user_id{nickname = Nick2},
     #user_id{nickname = Nick3}] = ets:tab2list(user_list),

    ClientPid1!stop,
    ClientPid2!stop,
    ClientPid3!stop,
    ClientPid4!stop,

    timer:sleep(500),
    ?_assertEqual(ets:tab2list(user_list), []).

add_user_update_nickname(_) ->
    Nick = "User1",
    NewNick = "User2",

    Pid = start_client(),

    ok = tcp_client:join_chat(Pid, Nick),
    timer:sleep(500),

    [#user_id{nickname = Nick}] = ets:tab2list(user_list),

    ok = tcp_client:update_nickname(Pid, NewNick),
    timer:sleep(500),

    [#user_id{nickname = NewNick}] = ets:tab2list(user_list),

    ok = tcp_client:close_client(Pid),
    timer:sleep(500),

    ?_assertEqual(ets:tab2list(user_list), []).

join_chat_send_message_normal(_) ->
    %%TODO: setup ip port via env
    Pid = start_client(),
    L = [
         tcp_client:join_chat(Pid, "Buddy"),
         tcp_client:send_message(Pid, "Hello!"),
         tcp_client:send_message(Pid, "How are you?"),
         tcp_client:quit_chat(Pid),
         tcp_client:close_client(Pid)],
    ?_assertEqual(L,[ok, ok, ok, ok, ok]).

store_history_test(_) ->
    Nick = "Buddy",
    Nick2 = "Guest",
    Message1 = "Hello",
    Message2 = "there",
    Pid = start_client(),
    ok = tcp_client:join_chat(Pid, Nick),
    ok = tcp_client:send_message(Pid, Message1),
    ok = tcp_client:send_message(Pid, Message2),
    timer:sleep(500),
    BinMessage1 = erlang:list_to_binary(Message1),
    BinMessage2 = erlang:list_to_binary(Message2),

    [#message{user_id = #user_id{nickname = Nick}, message_data = BinMessage1},
     #message{user_id = #user_id{nickname = Nick}, message_data = BinMessage2}] = ets:tab2list(chat_history),

    %test prepare_history
    Pid2 = spawn_join_client(Nick2),
    timer:sleep(1000),

    Pid2!stop,
    ok = tcp_client:close_client(Pid),
    timer:sleep(500),
    ?_assertEqual(ets:tab2list(user_list), []).


%% ====================================================================
%% HELPER FUNCTIONS
%% ====================================================================

spawn_join_client(Nickname) ->
    Pid1 = spawn(fun() ->
                     {ok, Pid} = tcp_client:start_link(),
                     tcp_client:connect(Pid, {127,0,0,1}, 10001),
                     receive
                         {From, Ref, Nickname} -> From! {Ref, Pid}
                     end,
                     tcp_client:join_chat(Pid, Nickname)
                     %timer:sleep(infinity)
                end),
    Ref = make_ref(),
    Pid1!{self(), Ref, Nickname},
    receive
        {Ref, ClientPid} ->
            ClientPid
    end.

start_client() ->
    {ok, Pid} = tcp_client:start_link(),
    tcp_client:connect(Pid, {127,0,0,1}, 10001),
    Pid.

%tcp_client:join_chat(ClientPid1, Nick1)