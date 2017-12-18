-module(message_handler).

-export([handle_message/2]).

-include("server_manager.hrl").
-include("tcp_server.hrl").

handle_message(_Msg = <<?join_chat_req:8/integer , Data/binary >>,
               State = #state{state = ?connected,
                              socket = Socket}) ->
    ?FUNC_ENTER,
    io:format("Data: ~p~n",[binary_to_list(Data)]),
    case binary_to_list(Data) of
        [] ->
            send(Socket, ?common_message, "Please choose a nickname, it should be unique"),
            State;
        Nickname ->
            case gen_server:call(?server_mgr, {add_user, Socket, Nickname}) of
                ok ->
                    send(Socket, ?join_chat_ack, Nickname),
                    PreHistoryList = gen_server:call(?server_mgr, prepare_history),
                    case PreHistoryList of
                        [] ->
                            ok;
                        _ ->
                            [send(Socket, ?common_message, Msg) || Msg <- PreHistoryList]
                    end,
                    State#state{state = ?in_chat,
                                nickname = Nickname};
                {nok, nickname_in_use} ->
                    send(Socket, ?common_message, "This Nickname is already in use, "
                        "please try another one.~n"),
                    State;
                {nok, _Result} ->
                    State
            end
    end;

handle_message(_Msg = <<MsgId:8/integer, Data/binary>>,
               State = #state{nickname = Nickname,
                              state = ?in_chat,
                              socket = Socket}) ->
    ?FUNC_ENTER,
    case MsgId of
        ?common_message ->
            gen_server:cast(?server_mgr, {store_msg, Nickname, Socket, Data}),
            broadcast(Nickname, Socket, ?common_message, Data),
            State;
        ?update_nickname_req ->
            NewNickname = erlang:binary_to_list(Data),
            case gen_server:call(?server_mgr, {update_user, Socket, NewNickname, Nickname}) of
                ok ->
                    send(Socket,
                         ?update_nickname_ack,
                         NewNickname),
                    State#state{nickname = NewNickname};
                {nok, nickname_in_use} ->
                    send(Socket,
                         ?common_message,
                         "This Nickname is already in use, "
                         "please try another one"),
                    State
            end;
        ?quit_chat ->
            ok = gen_server:cast(?server_mgr, {delete_user, Nickname}),
            State#state{nickname = [], state = connected}
    end.

send(Socket, MsgId, Data) ->
    ?FUNC_ENTER,
    BinData = erlang:list_to_binary(Data),
    BinMessage = <<MsgId:8, BinData/binary>>,
    io:format("Sending message ~p to ~p~n",[BinMessage, Socket]),
    ok = gen_tcp:send(Socket, BinMessage),
    %ok = inet:setopts(Socket, [{active, once}]),
    ok.

broadcast(Nickname, OriginSocket, MessageType, Msg) ->
    ?FUNC_ENTER,
    FullMessage = Nickname ++ ": " ++ Msg,
    SocketList = gen_server:call(?server_mgr, get_user_list),
    io:format("Socketlist: ~p~n", [SocketList]),
    [send(Socket, MessageType, FullMessage) || Socket <- SocketList, Socket =/= OriginSocket].
