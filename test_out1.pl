vars([turn]).
arrays([wants_to_enter]).
program([
	assign(arr(wants_to_enter, pid), 1),
  	condGoto(turn = pid, 6),
  		condGoto(arr(wants_to_enter, 1-pid) = 1, 3),
  		assign(turn, pid),
  		goto(2),
  	sekcja,
  	assign(arr(wants_to_enter, pid), 0),
  	goto(1)
]).

%% Hyman's (incorrect)
%% Program jest niepoprawny.
%% Niepoprawny przeplot:
%% Proces 0: 1, 6, 6.
%% Proces 1: 1, 2, 6.
%% Procesy w sekcji: 0, 1.
