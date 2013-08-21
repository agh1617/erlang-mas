%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc Modul odpowiedzialny za logike pojedynczego agenta.

-module(agent).
-export([start/3, start/4]).
-record(arenas,{fight,reproduction,migration}).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec start(RingPid,BarPid,PortPid) -> ok
%% @doc Funkcja generujaca dane i startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac.
start(Ring,Bar,Port) ->
  S = genetic:solution(),
  Agent = {S,genetic:evaluation(S),config:initialEnergy()},
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas).

%% @spec start(RingPid,BarPid,PortPid,Agent) -> ok
%% @doc Funkcja startujaca danego agenta. W argumencie
%% adresy aren do ktorych agent ma sie zglaszac oraz dane agenta.
start(Ring,Bar,Port,Agent) ->
  Arenas = #arenas{fight = Ring, reproduction = Bar, migration = Port},
  loop(Agent,Arenas).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec loop(Agent,Arenas) -> ok
%% @doc Funkcja cyklu zycia agenta. Jego zachowanie jest zalezne od jego
%% energii. Rekurencja kreci sie w nieskonczonosc, poki energia nie osiagnie 0.
loop(Agent,Arenas) ->
  case emas_util:behavior(Agent) of
    death ->
      exit(normal);
    reproduction ->
      {Solution,Fitness,Energy} = Agent,
      NewEnergy = call({Solution,Fitness,Energy},Arenas#arenas.reproduction),
      loop({Solution,Fitness,NewEnergy},Arenas);
    fight ->
      {Solution,Fitness,Energy} = Agent,
      NewEnergy = call({Fitness,Energy},Arenas#arenas.fight),
      loop({Solution,Fitness,NewEnergy},Arenas);
    migration ->
      {Ring,Bar,Port} = call(emigration,Arenas#arenas.migration),
      loop(Agent,#arenas{fight = Ring, reproduction = Bar, migration = Port})
  end.

%% @spec call(Message,ArenaPid) -> Answer
%% @doc Funkcja wysyla podana wiadomosc do danej areny i zwraca otrzymana
%% odpowiedz.
call(Msg,ArenaPid) ->
  Ref = erlang:monitor(process, ArenaPid),
  ArenaPid ! {self(), Ref, Msg},
  receive
    {Ref, Ans} ->
      erlang:demonitor(Ref, [flush]),
      Ans;
    {'DOWN', Ref, process, ArenaPid, Reason} ->
      io:format("Arena do ktorej chce pisan proces ~p nie istnieje!~n",[self()]),
      erlang:error(Reason)
  after config:procesTimeout() -> % docelowo nie bedzie timeoutu
    io:format("Proces ~p nie doczekal sie odpowiedzi od areny!~n",[self()]),
    exit(timeout)
  end.