tower(N, T, C) :- 
    length(T, N),
    num_col(T, N),
    uniq(T, N),
    transpose(T, X),
    uniq(X, N),
    C = counts(Top, Bot, Left, Right),
    maplist(fd_labeling, T),
    count_dir(X, Top),
    rev_count(X, Bot),
    count_dir(T, Left),
    rev_count(T, Right).
    
num_col([], _).
num_col([H | T], N) :-
    length(H, N),
    num_col(T, N).

uniq([], _).
uniq([H | T], N) :-
    fd_domain(H, 1, N),
    fd_all_different(H),
    uniq(T, N).
    
% -------------------------------------------------------------------------------
% implementation for transpose used from TA Team Code Help, which itself is from
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% ------------------------------------------------------------------------------

rev_count(Grid, Dir) :-
    maplist(reverse, Grid, Revd),
    count_dir(Revd, Dir).

count_dir([], []).
count_dir([H | T], [Vh | Vt]) :-
    line_sight(H, 0, 0, Vh),
    count_dir(T, Vt).

line_sight([], _, VisCount, Vh) :-
    Vh is VisCount.
line_sight([H | T], Max, VisCount, Vh) :-
    H #> Max,
    Incr is VisCount + 1,
    line_sight(T, H, Incr, Vh).
line_sight([H | T], Max, VisCount, Vh) :-
    H #< Max,
    line_sight(T, Max, VisCount, Vh).



% Plain Tower

plain_tower(N, T, C) :-
    length(T, N),
    C = counts(Top, Bot, Left, Right),
    gen_list(Dflt, N),
    plain_check(T, Dflt, Left, Right),
    transpose(T, X),
    plain_check(X, Dflt, Top, Bot).

plain_check([], _, [], []).
plain_check([H | T], Dflt, [TLh | TLt], [BRh | BRt]) :-
    permutation(Dflt, H),
    plain_sight(H, 0, 0, TLh),
    plain_rev(H, BRh),
    plain_check(T, Dflt, TLt, BRt).

plain_rev(Grid, Dir) :-
    reverse(Grid, Revd),
    plain_sight(Revd, 0, 0, Dir).

plain_sight([], _, VisCount, DirRes) :-
    DirRes is VisCount.
plain_sight([H | T], Max, VisCount, DirRes):-
    H > Max,
    Incr is VisCount + 1,
    plain_sight(T, H, Incr, DirRes).
plain_sight([H | T], Max, VisCount, DirRes) :-
    H < Max,
    plain_sight(T, Max, VisCount, DirRes).
        
gen_list([], 0).
gen_list([H | T], N) :-
    H = N,
    Decr is N - 1,
    gen_list(T, Decr).


% Performance Testing

tower_time(Time) :-
    statistics(cpu_time, [Start|_]),
    tower(5, _, counts([2,1,4,3,2], [2,2,1,3,4], [2,5,3,1,3], [3,1,2,3,2])),
    statistics(cpu_time, [Finish|_]),
    Time is Finish - Start.

plain_tower_time(Time) :-
    statistics(cpu_time, [Start|_]),
    plain_tower(5, _, counts([2,1,4,3,2], [2,2,1,3,4], [2,5,3,1,3], [3,1,2,3,2])),
    statistics(cpu_time, [Finish|_]),
    Time is Finish - Start.

speedup(Ratio) :-
    tower_time(T),
    plain_tower_time(P),
    Ratio is P / T.


% Ambiguous Puzzle

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
