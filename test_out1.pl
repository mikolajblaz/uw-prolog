%% Hyman's (incorrect)
%% vars([turn]).
%% arrays([wants_to_enter]).
%% program([
%% 	assign(arr(wants_to_enter, pid), 1),
%%   	condGoto(turn = pid, 6),
%%   		condGoto(arr(wants_to_enter, 1-pid) = 1, 3),
%%   		assign(turn, pid),
%%   		goto(2),
%%   	sekcja,
%%   	assign(arr(wants_to_enter, pid), 0),
%%   	goto(1)
%% ]).
%%
%% Program jest niepoprawny.
%% Niepoprawny przeplot:
%% Proces 0: 1, 6, 6.
%% Proces 1: 1, 2, 6.
%% Procesy w sekcji: 0, 1.


%% Dekker's Algorithm (correct)
%% vars([turn]).
%% arrays([wants_to_enter]).
%% program([
%%     assign(arr(wants_to_enter, pid), 1),
%%     condGoto(arr(wants_to_enter, 1-pid) = 0, 8),
%%    		condGoto(turn = pid, 2),
%%    		assign(arr(wants_to_enter, pid), 0),
%%    		condGoto(turn <> pid, 5),
%%    		assign(arr(wants_to_enter, pid), 1),
%%    		goto(2),
%%    	sekcja,
%%    	assign(turn, 1 - pid),
%%    	assign(arr(wants_to_enter, pid), 0),
%%    	goto(1)
%% ]).
%%
%% Program jest poprawny (bezpieczny).
