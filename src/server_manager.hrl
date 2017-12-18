%%
%% Records
%%

-record(user_id, {nickname, socket}).
-record(message, {timestamp, user_id = #user_id{}, message_data}).


-define(server_mgr, whereis(server_mgr)).

-define(common_message, 1).
-define(update_nickname_req, 5).
-define(update_nickname_ack, 6).
-define(private_message, 10).
-define(status_message, 15).
-define(join_chat_req, 20).
-define(join_chat_ack, 21).
-define(quit_chat, 25).

-define(idle,idle).
-define(connected,connected).
-define(in_chat,in_chat).

-define(FUNC_ENTER, io:format("*** Enter ~p:~p~n",
                              [?MODULE, ?FUNCTION_NAME])).


-define(WRONG_STATE(MsgId, State),
        io:format("*** ~p:~p Message was received in wrong state."
                   " MessageType:~p State: ~w~n",
                   [?MODULE, ?FUNCTION_NAME, MsgId, State])).





