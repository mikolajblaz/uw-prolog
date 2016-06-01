vars([k]).
arrays([chce]).
program([assign(arr(chce, pid), 1), assign(k, pid),
condGoto(arr(chce, 1-pid) = 0, 5),
condGoto(k = pid, 3),
sekcja, assign(arr(chce, pid), 0), goto(1)]).

% vars([x, y]).
% arrays([t1, t2, t3]).
% program([assign(x, 1), sekcja, goto(1)]).

