-module(mas_hybrid_supervisor).
-behaviour(gen_server).

%% API
-export([start/0, register/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include ("mas.hrl").

-record(state, {islandPids :: list(pid()), worldPids :: list(pid())}).
-type state() :: #state{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start() -> ok.
start() ->
    lists:foreach(fun(Node) ->
      net_adm:ping(Node)
    end, net_adm:hosts_file()),
    case global:whereis_name(?MODULE) of
        undefined ->
            {ok, _} = gen_server:start({global, ?MODULE}, ?MODULE, [[]], []);
        _ -> ok
    end.

-spec register(IslandPids :: [pid()]) -> ok.
register(IslandPids) ->
  gen_server:cast(global:whereis_name(?MODULE), {register, self(), IslandPids}).

%% ====================================================================
%% Callbacks
%% ====================================================================
-spec init(Args :: term()) -> {ok, State :: state()} |
                              {ok, State :: state(), timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore.
init([]) ->
    {ok, #state{islandPids = [], worldPids = []}}.

-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
                         {reply, Reply :: term(), NewState :: state()} |
                         {reply, Reply :: term(), NewState :: state(),
                          timeout() | hibernate} |
                         {noreply, NewState :: state()} |
                         {noreply, NewState :: state(),
                          timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(),
                          NewState :: state()} |
                         {stop, Reason :: term(), NewState :: state()}.
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(Request :: term(), State :: state())
                 -> {noreply, NewState :: state()} |
                    {noreply, cleaning, timeout() | hibernate} |
                    {stop, Reason :: term(), NewState :: state()}.
handle_cast({register, From, NewIslandPids}, St = #state{islandPids = IslandPids, worldPids=WorldPids}) ->
    lists:foreach(fun(WorldPid) ->
      gen_server:cast(WorldPid, {new_islands, NewIslandPids})
    end, WorldPids),
    gen_server:cast(From, {new_islands, IslandPids}),
    {noreply, St#state{islandPids = [NewIslandPids | IslandPids], worldPids = [From | WorldPids]}}.

-spec handle_info(Info :: timeout() | term(), State :: state())
                 -> {noreply, NewState :: state()} |
                    {noreply, NewState :: state(), timeout() | hibernate} |
                    {stop, Reason :: term(), NewState :: state()}.
handle_info(theEnd, State) ->
    {stop, normal, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: state()) -> term().
terminate(_Reason, _) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()}, State :: state(),
                  Extra :: term()) ->
                         {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
