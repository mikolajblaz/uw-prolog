% Autor: Mikołaj Błaż
% Numer indeksu: 346862
% Data: 5.06.2016

?- consult('program1.pl').

?- ensure_loaded(library(lists)).


% replace0(+N, +List, +Elem, -NewList), jeśli NewList to lista List
% z N-tym elementem zamienionym na Elem (indeksowanie od 0)
replace0(N, L, E, NewL) :-
	nth0(N, L, _, Rest),			% usuwamy N-ty element
	nth0(N, NewL, E, Rest).		% wstawiamy Elem jako N-ty element


% Pomocnicze predykaty
vrb(V) :- vars(VS), member(V, VS).
arr(A) :- arrays(AS), member(A, AS).
instr(I, Instr) :- program(P), nth1(I, P, Instr).

readProgram(program(V, A, I)) :-
	vars(V), arrays(A), program(I).


% Stan wykonania programu jest reprezentowany jako term
%   state(VariableValues, ArrayValues, CounterValues)
% gdzie:
%   VariableValues to lista zmiennych i ich wartości:
%     [Var1-Val1, ..., VarM-ValM]
%   ArrayValues to lista tablic i ich wartości (a wartość jest listą liczb):
%     [Arr1-[A1_0, ..., A1_N], ..., ArrM-[AM_1, ..., AM_N]]
%   CounterValues to lista liczników instrukcji aktualnie wykonywanych
%   przez kolejne procesy:
%     [Pid1-C1, ..., PidN-CN]


 %%%%%%%%%%%%%%%%%%%%%%%%%%%% Inicjalizacja stanu %%%%%%%%%%%%%%%%%%%%%%%%%%%%


% createConstList(+N, +Val, -List), jeśli List jest
% N-elementową listą elementów Val
createConstList(0, _, []).
createConstList(N, Val, [Val|Tail]) :-
	N > 0,
	M is N-1,
	createConstList(M, Val, Tail).

% createAscendingList(+M, +N, -List), jeśli List jest listą [M, ..., N-1]
createAscendingList(N, N, []).
createAscendingList(M, N, [M|Tail]) :-
	M < N,
	M1 is M+1,
	createAscendingList(M1, N, Tail).

% initConstKeys(+Keys, +Val, -List), jeśli Keys jest listą [K1, ..., Kn],
% zaś List jest listą [K1-Val, ..., Kn-Val]
initConstKeys([], _, []).
initConstKeys([K|KS], Val, [K-Val|Tail]) :-
	initConstKeys(KS, Val, Tail).


% initState(+N, +Program, -InitState), jeśli InitState jest
% stanem początkowym programu Program wykonywanego przez N procesów
initState(N, program(Vars, Arrs, _), state(VarVals, ArrVals, CVals)) :-
	createConstList(N, 0, Zeros),
	createAscendingList(0, N, Pids),			% identyfikatory procesów [0..N-1]
	initConstKeys(Vars, 0, VarVals),	    % inicjalizujemy zmienne na 0
	initConstKeys(Arrs, Zeros, ArrVals),	% inicjalizujemy tablice na 0
	initConstKeys(Pids, 1, CVals).  			% inicjalizujemy liczniki na 1



%%%%%%%%%%%%%%%%%%%%%%%%%%%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
