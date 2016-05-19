% Autor: Mikołaj Błaż
% Numer indeksu: 346862
% Data: 5.06.2016

?- consult('program1.pl').

?- ensure_loaded(library(lists)).

% Pomocnicze predykaty
vrb(V) :- vars(VS), member(V, VS).
arr(A) :- arrays(AS), member(A, AS).
instr(I, Instr) :- program(P), nth1(I, P, Instr).


% Stan jest reprezentowany jako:
% - lista zmiennych i ich wartości ([zmienna:wartość])
% - lista tablic i ich wartości ([nazwaTablicy:[indeks:wartość]])
% - lista instrukcji aktualnie wykonywanych przez kolejne procesy ([proces-instrukcja])

% initProcesses(+N, -CountersList) :- CountersList jest N-elementową listą
% termów postaci "i:1", gdzie i to indeks elementu w liście (oraz pid procesu)
initProcesses(N, CountersList) :-
	integer(N),
	N >= 1,
	initProcesses(N, 0, CountersList).
% TODO: indeksowanie od 0 czy 1
initProcesses(N, N, []).
initProcesses(N, PID, [PID:1 | Counters]) :-
	PID < N,
	NewPID is PID+1,
	initProcesses(N, NewPID, Counters).


% initState(+N, +Program, -InitState).
