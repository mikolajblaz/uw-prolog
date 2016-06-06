% Autor: Mikołaj Błaż
% Numer indeksu: 346862
% Data: 5.06.2016

?- consult('program1.pl').

?- ensure_loaded(library(lists)).

% Pomocnicze predykaty

% replace0(+N, +List, +Elem, -NewList), jeśli NewList to lista List
% z N-tym elementem zamienionym na Elem (indeksowanie od 0)
replace0(N, L, E, NewL) :-
	nth0(N, L, _, Rest),			% usuwamy N-ty element
	nth0(N, NewL, E, Rest).		% wstawiamy Elem jako N-ty element


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
createConstList(0, _, []) :- !.
createConstList(N, Val, [Val|Tail]) :-
	N > 0,
	M is N-1,
	createConstList(M, Val, Tail).

% createAscendingList(+M, +N, -List), jeśli List jest listą [M, ..., N-1]
createAscendingList(N, N, []) :- !.
createAscendingList(M, N, [M|Tail]) :-
	M < N,
	M1 is M+1,
	createAscendingList(M1, N, Tail).

% initConstKeys(+Keys, +Val, -List), jeśli Keys jest listą [K1, ..., Kn],
% zaś List jest listą [K1-Val, ..., Kn-Val]
initConstKeys([], _, []) :- !.
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
% Definiujemy własny operator relacyjny '<>' o znaczeniu takim jak '=\='.
% Pozostałe operatory relacyjne: '=' oraz '<' mają znaczenie jak w Prologu
:- op(500, yfx, <>).
'<>'(A, B) :- A =\= B.

% evalSimple(+SimpleExp, +State, +Pid, -Num), jeśli SimpleExp jest
% wyrażeniem prostym, Num jego wartością w stanie wykonania programu State,
% o ile bieżący proces to Pid.
evalSimple(Num, _, _, Num) :-				% wyrażenie proste to liczba
	integer(Num),
	!.
evalSimple(pid, _, Pid, Pid) :- !.	% stała pid
evalSimple(Var, state(VarVals, _, _), _, Num) :-	% wyrażenie proste to zmienna
	Var \= pid,
	atom(Var),
	!,
	memberchk(Var-Num, VarVals).								% odczytujemy wartość zmiennej
evalSimple(arr(Arr, Exp), State, Pid, Num) :-	% wyrażenie proste to tablica
	evalExp(Exp, State, Pid, Index),						% obliczamy indeks tablicy
	State = state(_, ArrVals, _),
	memberchk(Arr-Vals, ArrVals),								% odczytujemy wartości tablicy
	nth0(Index, Vals, Num).											% odczytujemy szukaną wartość

% evalExp(+Exp, +State, +Pid, -Num), jesli wartość wyrażenia Exp
% obliczanego przez proces Pid w stanie State to Num
evalExp(Exp, State, Pid, Num) :-
	evalSimple(Exp, State, Pid, Num), !.	% jeśli Exp jest wyrażeniem prostym
evalExp(Exp, State, Pid, Num) :-
	Exp =.. [Oper, E1, E2],					% jeśli Exp jest wyrażeniem złożonym...
	Oper \= arr,										% ... ale nie tablicą
	evalSimple(E1, State, Pid, N1),	% obliczamy podwyrażenia
	evalSimple(E2, State, Pid, N2),
	NumExp =.. [Oper, N1, N2],			% konstruujemy obliczalne wyrażenie...
	Num is NumExp.									% i obliczamy je

% evalBoolExp(+BoolExp, +State, +Pid, -Bool), jesli wartość wyrażenia
% logicznego BoolExp w stanie State obliczanego przez proces Pid to Bool,
% gdzie Bool to stała true lub false.
evalBoolExp(BExp, State, Pid, Bool) :-
	BExp =.. [Oper, E1, E2],				% BExp jest wyrażeniem złożonym
	evalSimple(E1, State, Pid, N1),	% obliczamy podwyrażenia
	evalSimple(E2, State, Pid, N2),
	NBExp =.. [Oper, N1, N2],				% konstruujemy wyrażenie logiczne
	( NBExp
	-> Bool = true
	; Bool = false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%% Wykonanie instrukcji %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% step(+Program, +InState, ?Pid, -OutState), jeśli po wykonaniu przez
% proces Pid bieżącej instrukcji programu Program w stanie InState,
% stan wynikowy to OutState.
step(program(_, _, Instrs), InState, Pid, OutState) :-
	InState = state(_, _, CVals),
	memberchk(Pid-Counter, CVals),	% wartość licznika instrukcji procesu Pid
	nth1(Counter, Instrs, Instr),		% wykonywana instrukcja
	stepInstr(Instr, InState, Pid, OutState).


% stepInstr(+Instr, +InState, +Pid, -OutState), jeśli po wykonaniu przez
% proces Pid instrukcji Instr w stanie InState stan wynikowy to OutState.
stepInstr(assign(Var, Exp), InState, Pid, OutState) :-	% przypisanie
	atom(Var),
	!,
	InState = state(VarVals, ArrVals, CVals),

	evalExp(Exp, InState, Pid, Num),
	% przypisz obliczoną wartość Num do zmiennej Var:
	selectchk(Var-_, VarVals, Var-Num, NewVarVals),
	
	incrementCounter(CVals, Pid, NewCVals),				% bezwarunkowo zwiększ licznik
	OutState = state(NewVarVals, ArrVals, NewCVals).	% stan końcowy


stepInstr(assign(arr(Var, IndexExp), Exp), InState, Pid, OutState) :-
	InState = state(VarVals, ArrVals, CVals),

	evalExp(Exp, InState, Pid, Num),
	evalExp(IndexExp, InState, Pid, Index),				% oblicz wartość wyrażeń

	memberchk(Var-Arr, ArrVals),									% Arr to wartości tablicy
	replace0(Index, Arr, Num, NewArr),						% wstaw Num pod indeksem Index
	% przypisz zmienioną tablicę z powrotem do zmiennej Var:
	selectchk(Var-_, ArrVals, Var-NewArr, NewArrVals),
	
	incrementCounter(CVals, Pid, NewCVals),				% bezwarunkowo zwiększ licznik
	OutState = state(VarVals, NewArrVals, NewCVals).	% stan końcowy


stepInstr(goto(Num), InState, Pid, OutState) :-	% skok
	InState = state(VarVals, ArrVals, CVals),
	selectchk(Pid-_, CVals, Pid-Num, NewCVals),		% zmień wartość licznika na Num
	OutState = state(VarVals, ArrVals, NewCVals).


stepInstr(condGoto(BoolExp, Num), InState, Pid, OutState) :- % warunkowy skok
	InState = state(VarVals, ArrVals, CVals),

	evalBoolExp(BoolExp, InState, Pid, Bool),
	(Bool = true
	-> selectchk(Pid-_, CVals, Pid-Num, NewCVals)		% zadana instrukcja
	; incrementCounter(CVals, Pid, NewCVals)),			% następna instrukcja

	OutState = state(VarVals, ArrVals, NewCVals).


stepInstr(sekcja, InState, Pid, OutState) :- 		% sekcja krytyczna
	InState = state(VarVals, ArrVals, CVals),
	incrementCounter(CVals, Pid, NewCVals),				% po prostu następna instrukcja
	OutState = state(VarVals, ArrVals, NewCVals).



% incrementCounter(+CVals, +Pid, -NewCVals), jeśli w tablicy
% liczników instrukcji CVals, licznik odpowiadający procesowi Pid
% jest zwiększony o 1 w tablicy NewCVals.
incrementCounter(CVals, Pid, NewCVals) :-
	memberchk(Pid-Counter, CVals),
	NewCounter is Counter+1,
	selectchk(Pid-_, CVals, Pid-NewCounter, NewCVals).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sekcja krytyczna %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getProgramSections(+Program, +Sections), jeśli Sections jest listą
% numerów instrukcji 'sekcja' w programie Program.
getProgramSections(program(_, _, Instrs), Sections) :-
	getProgramSections(Instrs, Sections, 1).

% getProgramSections(+Instrs, +Sections, +N), jeśli Sections jest listą
% numerów instrukcji 'sekcja' w liście instrukcji Instrs,
% gdzie numerowanie instrukcji zaczyna się od N.
getProgramSections([], [], _) :- !.
getProgramSections([sekcja | Instrs], [N | Tail], N) :-
	!,
	M is N+1,
	getProgramSections(Instrs, Tail, M).
getProgramSections([I | Instrs], Tail, N) :-
	I \= sekcja,
	M is N+1,
	getProgramSections(Instrs, Tail, M).


% twoInSection(+Sections, +CVals, -[Pr1, Pr2]), jeśli Pr1, Pr2 to najmniejsze
% dwóch procesów, które mogą wejść do którejś sekcji krytycznej z listy
% Sections, czyli ich liczniki z listy CVals są numerami instrukcji 'sekcja'.
twoInSection(Sections, [PrId-C | CVals], [PrId1, PrId2]) :-
	% jeśli C to 'sekcja', to sprawdź czy jest jeszcze 1 proces w sekcji
	(memberchk(C, Sections)
	-> PrId = PrId1, anyInSection(Sections, CVals, PrId2)
	; twoInSection(Sections, CVals, [PrId1, PrId2])).

% anyInSection(+Sections, +CVals, -PrInSec), jeśli proces PrInSec
% może wejść do którejś sekcji krytycznej z listy Sections.
anyInSection(Sections, [PrId-C | _], PrId) :-
	memberchk(C, Sections), !.
anyInSection(Sections, [_-C | CVals], PrInSec) :-
	\+memberchk(C, Sections),
	anyInSection(Sections, CVals, PrInSec).



%%%%%%%%%%%%%%%%%%%%%%%%% Sprawdzanie bezpieczeństwa %%%%%%%%%%%%%%%%%%%%%%%%%%
% checkSafety(+N, +Program, +State, -Safety), jeśli stan State
% programu Program dla N procesów nie jest bezpieczny
checkSafety(N, Program, State, Safety) :-
	getProgramSections(Program, Sections),
	checkSafety(N, Program, 0, State, [State], Sections, _, Safety).
	
% checkSafety(+N, +Program, +Pid,     +State, +CheckedStates, +Sections, +NewCheckedStates), jeśli stan State
% programu Program dla N procesów nie jest bezpieczny, lista CheckedState jest
% listą odwiedzonych stanów (a jej pierwszy element to stan State),
% a Sections jest listą numerów instrukcji 'sekcja' w programie.
checkSafety(N, _, N, _, CheckedStates, _, CheckedStates, safe) :- !.


checkSafety(N, Program, Pid, State, ChkStates, Sections, NewChkStates, Safety) :-
	Pid < N,
	State = state(_, _, CVals),
	(twoInSection(Sections, CVals, InSec)
	-> Safety = unsafe(InSec, []),
		NewChkStates = ChkStates
	;	NextPid is Pid+1,

		step(Program, State, Pid, NextState),			% wybierz następny stan
		(memberchk(NextState, ChkStates)
		% jeśli był odwiedzony, przejrzyj następnych sąsiadów)
		-> checkSafety(N, Program, NextPid, State, ChkStates, Sections, NewChkStates, Safety)
		% jesli nie
		% sprawdź bezpiecześntwo w nowym stanie, po dodaniu go do listy odwiedzonych:
		; checkSafety(N, Program, 0, NextState, [NextState | ChkStates], Sections, SubtreeChkStates, SubtreeSafety),
		% po przejrzeniu stanów potomnych, wywołaj się dla bieżącego stanu, ale następnego procesu.

			(SubtreeSafety = safe
			-> checkSafety(N, Program, NextPid, State, SubtreeChkStates, Sections, NewChkStates, Safety)
			; SubtreeSafety = unsafe(InSec, PidList),
				memberchk(Pid-C, CVals),
				Safety = unsafe(InSec, [Pid-C | PidList]),
				NewChkStates = SubtreeChkStates)
		)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Interakcja %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify(N, _) :-
	N < 1,
	!,
	format('Error: parametr ~p powinien być liczbą dodatnią.~n', [N]).
verify(N, ProgramFile) :-
	N >= 1,
	(readProgram(ProgramFile, Program)	% wczytaj program z pliku
	-> initState(N, Program, InitState),
		checkSafety(N, Program, InitState, Safety),
		printResult(Safety)
	;	format('Error: niepoprawna nazwa pliku - ~p.~n', [ProgramFile])).


readProgram(ProgramFile, program(Vars, Arrays, Instrs)) :-
	set_prolog_flag(fileerrors, off),
	see(ProgramFile),
	!,
	read(vars(Vars)),
	read(arrays(Arrays)),
	read(program(Instrs)),
	seen.


printResult(safe) :-
	!,
	format('Program jest poprawny (bezpieczny).~n').

printResult(unsafe(InSec, InstrList)) :-
	length(InstrList, VisitedCnt),
	StateCnt is VisitedCnt + 1,
	format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.~n', [StateCnt]),
	printProcesses(InstrList),
	format('Procesy w sekcji: ~p, ~p ~n', InSec).

printProcesses([]) :- !.
printProcesses([Pid-C | InstrList]) :-
	format('Proces ~d: ~d~n', [Pid, C]),
	printProcesses(InstrList).
	
	
