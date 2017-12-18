%% @author kintu


-module(server_manager).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-include("server_manager.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([add_user/2]).
%% ====================================================================
%% start_link/1
%% ====================================================================
start_link() ->
    ?FUNC_ENTER,
    gen_server:start_link(?MODULE, [], []).

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
    register(server_mgr, self()),
    ets:new(user_list,[set, named_table, {keypos, 2}]),
    ets:new(chat_history,[ordered_set, named_table, {keypos, 2}]),
    {ok, #state{}}.

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
handle_call(prepare_history, _From, State) ->
    ?FUNC_ENTER,
    Reply = prepare_history(),
    {reply, Reply, State};

handle_call({add_user, Socket, Nickname}, _From, State) ->
    ?FUNC_ENTER,
    Result = add_user(Socket, Nickname),
    {reply, Result, State};

handle_call({update_user, Socket, NewNickname, OldNickname}, _From, State) ->
    ?FUNC_ENTER,
    Result = update_user(Socket, NewNickname, OldNickname),
    {reply, Result, State};

handle_call(get_user_list, _From, State) ->
    ?FUNC_ENTER,
    Result = get_user_list(),
    {reply, Result, State};

handle_call(Msg, _From, State) ->
    ?FUNC_ENTER,
    io:format("Wrong request, Message was: ~p~n",[Msg]),
    {reply, {nok, wrong_request}, State}.


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
handle_cast({delete_user, Nickname}, State) ->
    ?FUNC_ENTER,
    delete_user(Nickname),
    {noreply, State};

handle_cast({store_msg, Nickname, Socket, Data}, State) ->
    ?FUNC_ENTER,
    store_message(erlang:timestamp(),
                  #user_id{nickname = Nickname,socket = Socket},
                  Data),
    {noreply, State};

handle_cast(stop, State) ->
    ?FUNC_ENTER,
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

add_user(Socket, Nickname) ->
    ?FUNC_ENTER,
    case ets:insert_new(user_list, #user_id{nickname = Nickname, socket = Socket}) of
        true ->
            ok;
        _ ->
            {nok, nickname_in_use}
    end.

update_user(Socket, NewNickname, OldNickname) ->
    ?FUNC_ENTER,
    case add_user(Socket, NewNickname) of
        ok ->
            delete_user(OldNickname);
        _ ->
            {nok, nickname_in_use}
    end.

delete_user([]) ->
    ?FUNC_ENTER,
    ok;

delete_user(Nickname) ->
    ?FUNC_ENTER,
    io:format("Deleting ~p from user table~n", [Nickname]),
    case ets:delete(user_list, Nickname) of
        true ->
            ok;
        _ ->
            {nok, internal_error}
    end.

get_user_list() ->
    ?FUNC_ENTER,
    [Socket || #user_id{socket = Socket} <- ets:tab2list(user_list)].

store_message(Timestamp, UserId, Data) ->
    ets:insert_new(chat_history, #message{timestamp = Timestamp,
                                          user_id = UserId,
                                          message_data = Data}).

prepare_history() ->
    First = ets:first(chat_history),
    case First of
        '$end_of_table' ->
            [];
        _ ->
            KeyList = lists:foldl(fun(_Iter, Acc) ->
                                        case
                                            ets:next(chat_history, hd(Acc)) of
                                            '$end_of_table' ->
                                                Acc;
                                             Next ->
                                                [Next|Acc]
                                        end
                                    end
                                   ,[First], lists:seq(1, 10)),
            lists:map(fun(Elem) ->
                              [#message{message_data = Message,
                                        user_id = #user_id{nickname = Nickname}}] = ets:lookup(chat_history, Elem),
                              Nickname ++ ": " ++ erlang:binary_to_list(Message)
                      end, lists:reverse(KeyList))
    end.


