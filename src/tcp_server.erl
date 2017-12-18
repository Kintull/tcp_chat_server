%% @author kintull
%% @doc @todo Add description to tcp_server.


-module(tcp_server).
-behaviour(gen_server).
-include("server_manager.hrl").
-include("tcp_server.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% ====================================================================
%% start_link/1
%% ====================================================================
start_link(Socket) ->
    ?FUNC_ENTER,
    gen_server:start_link(?MODULE, Socket, []).


%% ====================================================================
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(Socket) ->
    ?FUNC_ENTER,
    %process_flag(trap_exit, true),
    gen_server:cast(self(),accept),
    {ok, #state{socket = Socket}}.


%% ====================================================================
%% handle_call/3
%% ====================================================================
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    ?FUNC_ENTER,
    Reply = ok,
    {reply, Reply, State}.



%% ====================================================================
%% handle_cast/2
%% ====================================================================
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(accept, State = #state{socket = ListenSocket, state = idle}) ->
    ?FUNC_ENTER,
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    tcp_server_sup:start_socket(),
    %ok = inet:setopts(AcceptSocket, [{active, once}]),
    {noreply, State#state{socket = AcceptSocket, state = connected}};

handle_cast(stop, State = #state{nickname = Nickname, socket = Socket}) ->
    ?FUNC_ENTER,
    gen_tcp:close(Socket),
    gen_server:cast(?server_mgr, {delete_user, Nickname}),
    {stop, normal, State}.

%% ====================================================================
%% handle_info/2
%% ====================================================================
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({tcp, _Socket, Str}, State = #state{socket = _Socket}) ->
    ?FUNC_ENTER,
    io:format("Received Message ~p~n",[Str]),
    NewState = message_handler:handle_message(Str, State),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State = #state{nickname = Nickname, socket = _Socket}) ->
    ?FUNC_ENTER,
    gen_server:cast(?server_mgr, {delete_user, Nickname}),
    {noreply, State}.

%% ====================================================================
%% terminate/2
%% ====================================================================
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(Reason, State = #state{nickname = Nickname}) ->
    io:format("*** ~p:~p Reason ~p State ~p~n",[?MODULE, ?FUNCTION_NAME,
                                                Reason, State]),
    gen_server:cast(?server_mgr, {delete_user, Nickname}),
    ok.


%% ====================================================================
%% code_change/3
%% ====================================================================
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


