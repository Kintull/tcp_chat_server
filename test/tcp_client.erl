%% @author kintu
%% @doc @todo Add description to tcp_client.


-module(tcp_client).
-behaviour(gen_server).

-define(client, client).

-define(common_message, 1).
-define(update_nickname_req, 5).
-define(update_nickname_ack, 6).
-define(private_message, 10).
-define(status_message, 15).
-define(join_chat_req, 20).
-define(join_chat_ack, 21).
-define(quit_chat, 25).


-define(WRONG_STATE(MsgId, State),
        io:format("*** ~p:~p Message was received in wrong state."
                  " MessageType:~p State: ~w~n",
                  [?MODULE, ?FUNCTION_NAME, MsgId, State])).

-define(FUNC_ENTER, io:format("*** Enter ~p:~p~n",[?MODULE, ?FUNCTION_NAME])).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         start/0,
         connect/3,
         join_chat/2,
         quit_chat/1,
         send_message/2,
         close_client/1,
         update_nickname/2,
         send/3]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket, state = idle, nickname}).


%% ====================================================================
%% start_link/0
%% ====================================================================
start_link() ->
    ?FUNC_ENTER,
    gen_server:start_link(?MODULE, [], []).

start() ->
    ?FUNC_ENTER,
    gen_server:start(?MODULE, [], []).

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
init([]) ->
    ?FUNC_ENTER,
    %register(Pid, self()),
    {ok, #state{}}.


%% ====================================================================
%% handle_call/3
%% ====================================================================
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: term()) -> Result when
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
handle_call({connect, Ip, Port}, _From, State = #state{state = idle}) ->
    ?FUNC_ENTER,
    {ok, ClientSocket} = gen_tcp:connect(Ip, Port, []),
    ok = inet:setopts(ClientSocket, [{active, true},
                                     {packet, 0},
                                     {mode, binary}]),
    {reply, {ok, ClientSocket}, State#state{socket = ClientSocket, state = connected}};

handle_call({join_chat, Nickname}, _From,
            State = #state{socket = Socket, state = Current_state}) when Current_state == connected;
                                                                         Current_state == join_chat_requested ->
    ?FUNC_ENTER,
    %% TODO: add try
    Result = send(Socket, ?join_chat_req, Nickname),
    {reply, Result, State#state{state = join_chat_requested}};

handle_call(quit_chat, _From, State = #state{state = Current_state,
                                            socket = Socket,
                                            nickname = Nickname}) when Current_state == joined_chat;
                                                                       Current_state == join_chat_requested ->
    ?FUNC_ENTER,
    Result = send(Socket, ?quit_chat, Nickname),
    {reply, Result, State#state{state = connected}};

handle_call({send_msg, Msg}, _From, State = #state{state = joined_chat,
                                                  socket = Socket}) ->
    ?FUNC_ENTER,
    Result = send(Socket, ?common_message, Msg),
    {reply, Result, State};

handle_call({update_nickname, NewNickname}, _From, State = #state{state = joined_chat,
                                                  socket = Socket}) ->
    ?FUNC_ENTER,
    Result = send(Socket, ?update_nickname_req, NewNickname),
    {reply, Result, State}.


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
handle_cast(stop, State = #state{socket = Socket}) ->
    ?FUNC_ENTER,
    gen_tcp:close(Socket),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.




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
handle_info({tcp, _Socket, Str = <<MsgId:8/integer, Data/binary>>},
            State = #state{socket = _Socket, state = join_chat_requested}) ->
    ?FUNC_ENTER,
    %io:format("Received Message ~p~n",[Str]),
    case MsgId of
        ?join_chat_ack ->
            Nickname = erlang:binary_to_list(Data),
            io:format("You joined the chat ~n"),
            {noreply, State#state{nickname = Nickname, state = joined_chat}};
        ?common_message ->
            Message = erlang:binary_to_list(Data),
            io:format("~s~n",[Message]),
            {noreply, State};
        _ ->
            ?WRONG_STATE(MsgId, State)
   end;

handle_info({tcp, _Socket, Str = <<MsgId:8/integer, Data/binary>>},
    State = #state{socket = _Socket, state = joined_chat}) ->
    ?FUNC_ENTER,
    %io:format("Received Message ~p~n",[Str]),
    case MsgId of
        ?common_message ->
            io:format("~s~n",[erlang:binary_to_list(Data)]),
            {noreply, State};
        ?update_nickname_ack ->
            {noreply, State#state{nickname = erlang:binary_to_list(Data)}};
        _ ->
            ?WRONG_STATE(MsgId, State),
            {noreply, State}
    end;


handle_info({tcp_closed, _Socket}, State) ->
    ?FUNC_ENTER,
    %io:format("Received Message ~p~n",[Str]),
    {stop, tcp_closed, State};

handle_info(stop, State) ->
    ?FUNC_ENTER,
    %io:format("Received Message ~p~n",[Str]),
    {stop, normal, State}.
%% ====================================================================
%% terminate/2
%% ====================================================================
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ?FUNC_ENTER,
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

send(Socket, MsgType, Data) ->
    ?FUNC_ENTER,
    BinData = erlang:list_to_binary(Data),
    BinMessage = <<MsgType:8, BinData/binary>>,
    %io:format("MsgType: ~p Data: ~p~nBinary: ~p~n",[MsgType, Data, BinMessage]),
    ok = gen_tcp:send(Socket, BinMessage),
    ok.


%% ====================================================================
%% External functions
%% ====================================================================

connect(Pid, Ip, Port) ->
    timer:sleep(200),
    gen_server:call(Pid, {connect, Ip, Port}).

join_chat(Pid, Nickname) ->
    gen_server:call(Pid, {join_chat, Nickname}),
    timer:sleep(200),
    ok.


update_nickname(Pid, Nickname) ->
    gen_server:call(Pid, {update_nickname, Nickname}),
    timer:sleep(200),
    ok.

quit_chat(Pid) ->
    timer:sleep(200),
    gen_server:call(Pid, quit_chat).

send_message(Pid, Msg)->
    timer:sleep(200),
    gen_server:call(Pid, {send_msg, Msg}).

close_client(Pid) ->
    timer:sleep(200),
    gen_server:cast(Pid, stop).
