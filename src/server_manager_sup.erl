%% @author kintu
%% @doc @todo Add description to server_manager_sup.


-module(server_manager_sup).
-behaviour(supervisor).
-export([init/1]).
-include("server_manager.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link() ->
    ?FUNC_ENTER,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
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
init(_Args) ->
    ?FUNC_ENTER,
    AChild = {'server_manager',
              {'server_manager',start_link,[]},
              permanent,
              2000,
              worker,
              ['server_manager']},
    {ok,{{one_for_all,1,5}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


