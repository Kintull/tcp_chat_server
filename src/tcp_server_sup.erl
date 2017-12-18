%% @author kintull
%% @doc @todo Add description to tcp_server_sup.


-module(tcp_server_sup).
-behaviour(supervisor).
-export([init/1]).
-include("server_manager.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         start_socket/0]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
%% start_link/0
%% ====================================================================
start_link() ->
    ?FUNC_ENTER,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% init/1
%% ====================================================================

-spec init(Args :: term()) -> Result when
    Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
    SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
    RestartStrategy :: one_for_all
                     | one_for_one
                     | rest_for_one
                     | simple_one_for_one,
    ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
    StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
    RestartPolicy :: permanent
                   | transient
                   | temporary,
    Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    ?FUNC_ENTER,
    %{ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(10001,
                            [{active, true},
                             {reuseaddr, true},
                             {packet, 0},
                             {mode, binary}]),
    spawn_link(fun prepare_listeners/0),
    AChild = {'tcp_server',
              {'tcp_server',start_link,[ListenSocket]},
              temporary,
              1000,
              worker,
              ['tcp_server']},
    {ok,{{simple_one_for_one, 60, 3600}, [AChild]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_socket() ->
    ?FUNC_ENTER,
    supervisor:start_child(?MODULE, []).

prepare_listeners() ->
    ?FUNC_ENTER,
    start_socket(),
    %[start_socket() || _ <- lists:seq(1,20)],
    ok.
