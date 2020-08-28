% Suduku
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(between)).
% consistent(+SGrid): For all areas of the Sudoku grid SGrid it holds
% that all positive integers in the area are distinct.

consistent(SGrid):-
    rowsOk(SGrid),
    transcheckcolumns(SGrid),
    boxesOk(SGrid).


%rowsOk(+LofLists)Takes in a list of lists, and checks if each row is ok utalizing tail recursion
rowsOk([]):- true.
rowsOk([Head|Tail]):-
    rowOk(Head),
    rowsOk(Tail).

%rowOk(+List) All elements except for 0s are unique % TODO: REMOVE 0s
rowOk(List):-
    findall(X,(member(X,List),X\==0),ListSZ),
    sort(ListSZ,SLSZ),
    length(ListSZ,L1),
    length(SLSZ,L2),
    L1=L2.

    
%transcheckcolumns(+LofLists)- takes an SGrid and checks if each column is ok
transcheckcolumns(SGrid):-
    transpose(SGrid,CGrid),
    rowsOk(CGrid).

% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).


% The first N elements of a List.
firstn(_N,[],[]).
firstn(N,[Head|Tail],Result):-
    (N = 1
    ->  Result = [Head]
    ; N1 is N-1, firstn(N1,Tail,R1), append([Head],R1,Result)
    ).

chop(N,List,LofLists):-
    (List = []
    ->  LofLists = []
    ; firstn(N,List,Slice), 
    append(Slice,Tail,List), 
    chop(N,Tail,LoL1), 
   	append([Slice],LoL1,LofLists)
    ).   

%trans(LoL,LoL). TODO: Implement Tail Recursion: BaseCase?


%boxesOk(+LofLists):- takes an Sgrid and checks to see if each subgrid box is ok!
boxesOk(SGrid):- 
    length(SGrid,N),
    SqrtN is round(sqrt(N)),
    chop(SqrtN,SGrid,ChoppedG),
   	findall(TL,(member(X,ChoppedG), transpose(X,TL)),TChG),
    append(TChG,Lol1),
    append(Lol1,Lol2),
    chop(N,Lol2,PSL),
    rowsOk(PSL).
    

% sudoku0(+Grid0, ?Grid): Grid is a complete refinement of the Sudoku grid Grid0
sudoku0(SGrid, ResultSGrid):-
    %Check if SGrid is consistent
    consistent(SGrid),
    length(SGrid,N),
    %Generate All Possible Complete Refinements of SGrid G1 and G1 is consistent
    %Replace one 0 in the Grid
    (replacefirst(SGrid,X,G1) ->
    between(1,N,X),
    sudoku0(G1, ResultSGrid)
    ; ResultSGrid = SGrid
    ).

replacefirst(G0,X,RG):-
    %Flatten to one list
    length(G0,N),
    append(G0,LFG0),
    rpz(LFG0,X,R),
    chop(N,R,RG).

rpz([Head|Tail],X,Result):-
    (   Head = [] ->  
    Result = []
    ; 
    Head = 0 ->  
   	append([X],Tail,Result)
    ; 
    rpz(Tail,X,R1),
    append([Head],R1,Result)
    ).
    
    
    
    
   
    
    
    
    
    
    
    
    
    
    