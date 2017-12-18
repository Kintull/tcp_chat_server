%% @author kintu
%% @doc @todo Add description to main.


-module(main).
-behaviour(application).
-export([start/2, stop/1]).
-include("server_manager.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
    {ok, Pid :: pid()}
    | {ok, Pid :: pid(), State :: term()}
    | {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
    ?FUNC_ENTER,
    {ok, ServerPid} = tcp_server_sup:start_link(),
    {ok, _ServerManagerPid} = server_manager_sup:start_link(),
    io:format("Server started.~n"),
    {ok, ServerPid}.


%% ====================================================================
%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


