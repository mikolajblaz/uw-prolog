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


 %%%%%%%%%%%%%%%%%%%%%%%%%%%% Inicjalizacja stanu %%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ewaluacja wyrażeń %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definiujemy własny operator logiczny '<>' o znaczeniu takim jak '=\='.
% Pozostałe operatory logiczne: '=' oraz '<' mają znaczenie jak w Prologu
:- op(500, yfx, <>).
'<>'(A, B) :- A =\= B.

% evalSimple(+SimpleExp, +State, -Num), jeśli SimpleExp jest wyrażeniem prostym,
% Num jego wartością, a State stanem wykonania programu.
evalSimple(Num, _, Num) :-	% wyrażenie proste to liczba
	integer(Num).
evalSimple(Var, state(VarVals, _, _), Num) :-	% wyrażenie proste to zmienna
	atom(Var),
	memberchk(Var-Num, VarVals).								% odczytujemy wartość zmiennej
evalSimple(arr(Arr, Exp), State, Num) :-			% wyrażenie proste to tablica
	evalExp(Exp, State, Index),									% obliczamy indeks tablicy
	State = state(_, ArrVals, _),
	memberchk(Arr-Vals, ArrVals),								% odczytujemy wartości tablicy
	nth0(Index, Vals, Num).											% odczytujemy szukaną wartość

% evalExp(+Exp, +State, -Num), jesli wartość wyrażenia
% Exp w stanie State to Num
evalExp(Exp, State, Num) :-
	evalSimple(Exp, State, Num).		% sprawdzamy czy Exp jest wyrażeniem prostym
evalExp(Exp, State, Num) :-
	Exp =.. [Oper, E1, E2],					% sprawdzamy czy Exp jest wyrażeniem złożonym
	Oper \= arr,										% ... ale nie tablicą
	evalSimple(E1, State, N1),			% obliczamy podwyrażenia
	evalSimple(E2, State, N2),
	NumExp =.. [Oper, N1, N2],			% konstruujemy obliczalne wyrażenie...
	Num is NumExp.									% i obliczamy je

% evalBoolExp(+BoolExp, +State, -Bool), jesli wartość wyrażenia logicznego
% BoolExp w stanie State to Bool, gdzie Bool to stała true lub false.
evalBoolExp(BExp, State, Bool) :-
	BExp =.. [Oper, E1, E2],				% BExp jest wyrażeniem złożonym
	evalSimple(E1, State, N1),			% obliczamy podwyrażenia
	evalSimple(E2, State, N2),
	NBExp =.. [Oper, N1, N2],				% konstruujemy wyrażenie logiczne
	( NBExp
	-> Bool = true
	; Bool = false).


