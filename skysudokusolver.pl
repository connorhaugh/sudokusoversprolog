:- use_module(library(clpfd)).

% Comment out the next 17 lines, before submitting to ETS. Works in SWI

domain(L, Min, Max) :-
	L ins Min..Max.

:- op(760,  yfx, #<=>).
:- op(750,  xfy, #=>).
:- op(750,  yfx, #<=).

X #<=> Y :- X #<==> Y.

X #=> Y :-  X #==> Y.

X #<= Y :-  X #<== Y.

table(Tuples, Table) :-
       tuples_in(Tuples, Table).

% end of text to be commented out


% visnum(+L, ?K): the number of elements in list L that are visible from the left is K.
% It can be assumed that L is proper (i.e., not open-ended). The elements of L as well as K are
% FD-variables or integers. visnum/2 should not perform labeling nor create any choice points.
visnum([H|T],K):-
    visnum(H,T,K0),
    K #= K0+1.
visnum(_PM,[],0).
visnum(PM,[H|T],K):-
    max(PM,H) #= LM,
    visnum(LM,T,K0),
    H#>PM #<=> B,
    K#= K0+ B.

length_list(N,L):
	length(L,N).
oreintlist(Grid,Dir,RC,Dlist):-
    Dir #= n  #<=> N %North
    Dir #= s  #<=> S %South
    Dir #= w  #<=> W %West
    Dir #= e  #<=> E %East

    %ToDo: Add the directionflipping

applygclue(Grid,GClue):-
    g(N,R,C)=GClue,
    nth(C,Grid,Row),
    nth(R,Grid,Elem),
    Elem #= N.

applyvclue(Grid,VClue):-
    v(N,Dir,RC)=Clue,
    orientlist(Grid,Dir,RC,Dlist),
    visnum(Dlist,N).

% skysudoku(+SP, ?SS): SS is a solution of the Skyscraper Sudoku puzzle SP

skysudoku(SkyPuzzle, SkySolution):-
    ss(N,Clues)=SkyPuzzle,
    include(=(g(_,_,_)),Clues,GClues),
    include(=(v(_,_,_)),Clues,VClues),
    length_list(N,Grid),
    maplist(length_list(N),Grid),

    %apply Gclues
    maplist(applygclue(Grid),GClues)

    %sudoku general sudoku things
    maplist()
    maplist(all_distinct(),Grid),
    append(Grid,FGrid),
    domain(FGrid,1,N),
    transpose(FGrid,TFGrid),
    maplist(all_distinct(),Grid),
    %TODO: squares
    %apply Vclues
    maplist(applyvclue(Grid),VClues),

    SkySolution=Grid
