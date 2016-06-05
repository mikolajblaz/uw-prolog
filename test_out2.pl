vars([turn]).
arrays([wants_to_enter]).
program([
    assign(arr(wants_to_enter, pid), 1),
    condGoto(arr(wants_to_enter, 1-pid) = 0, 8),
   		condGoto(turn = pid, 2),
   		assign(arr(wants_to_enter, pid), 0),
   		condGoto(turn <> pid, 5),
   		assign(arr(wants_to_enter, pid), 1),
   		goto(2),
   	sekcja,
   	assign(turn, 1 - pid),
   	assign(arr(wants_to_enter, pid), 0),
   	goto(1)
]).

%% Dekker's Algorithm (correct)
%% Program jest poprawny (bezpieczny).
