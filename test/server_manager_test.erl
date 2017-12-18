-module(server_manager_test).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(server_mgr, server_manager).

%% ====================================================================
%% TESTS DESCRIPTIONS
%% ====================================================================

start_stop_test_() ->
{"Add 3 users, delete 3 users",
 ?setup(fun add_delete_normal/1)}.

%% ====================================================================
%% SETUP FUNCTIONS
%% ====================================================================

start() ->

    {ok, Pid}= server_manager:start_link(),
    ok.

stop(Pid) ->
    gen_server:cast(?server_mgr, stop),
    Pid.

%% ====================================================================
%% TESTS
%% ====================================================================

%test_user_table({_ServerPid, _ClientPid, _ListenSocket})>
%case ase gen_server:call(?server_mgr, {check_nickname, Sock"", "User1"})

add_delete_user_normal(_) ->
    %%TODO: setup ip port via env

   tcp_server:call(?server_mgr, add_user)


%% ====================================================================
%% HELPER FUNCTIONS
%% ====================================================================

start_client() ->
    tcp_client:start_link(),
    tcp_client:connect({127,0,0,1}, 10001),
    ok.