:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState([ [['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', ''],
               ['', '', '', '', '', '', '', '', '']],
               first]).

%getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).

getBoards(S, B) :- nth0(0, S, B).
getLastMove(S, Move):- nth0(1, S, Move).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT,
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(State,UPos, Board) :- positions(Positions),
    nth0(Index,Positions, UPos), getBoards(State, BoardU),
    nth0(Index, BoardU, Board).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).

getUBoard(State,UboardState) :- getBoards(State, UBoard),
    myFunction(UBoard,UboardState).

myFunction([],[]).
myFunction([H|T],  [HRez|TRez]):-
    getBoardResult(H, HRez),
    myFunction(T, TRez).


% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(State, UPos, Pos, Cell) :- positions(Positions),
    nth0(IndexU, Positions, UPos), getBoards(State, BoardU),
    nth0(IndexU, BoardU, BoardIndv), nth0(IndexI,Positions,Pos),
    nth0(IndexI, BoardIndv, Cell).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos,
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(Positions),
     nth0(Index, Positions, Pos), nth0(Index, Board, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(State, NextPlayer) :- getBoards(State, Board),
    aparitiiUTT(TotX, Tot0, Board),
    myNextPlayer(TotX, Tot0, NextPlayer).

aparitiiUTT(0,0,[]).
aparitiiUTT(TotX, Tot0, [H|T]):-
    aparitiiBoard(NrX,Nr0,H),
    aparitiiUTT(NewTotX, NewTot0,T),
    TotX is NewTotX + NrX,
    Tot0 is NewTot0 + Nr0,!
   .

aparitiiBoard(0, 0, []).
aparitiiBoard(NrX, Nr0, [x|T]):-
    aparitiiBoard(NewNrX, Nr0, T),
    NrX is NewNrX + 1,!.
aparitiiBoard(NrX, Nr0, [0 |T]):-
    aparitiiBoard(NrX, NewNr0, T),
    Nr0 is NewNr0 + 1, !.
aparitiiBoard(NrX, Nr0, [_|T]):-
    aparitiiBoard(NrX, Nr0, T), !.

myNextPlayer(NrX, Nr0, NextPlayer):- NrX == Nr0,
    NextPlayer = x,!.
myNextPlayer(NrX, Nr0, NextPlayer):- NrX > Nr0,
    NextPlayer = 0,!.


% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile
% din UBoard ale tablelor disponibile pentru următoarea mutare.
getNextAvailableBoards(State, NextBoardsPoss):- initialState(State),
                       positions(NextBoardsPoss),!.
getNextAvailableBoards(State, [LastMove]):-
   getLastMove(State, LastMove),
   getUBoard(State, UBoard),
   getBoardResult(UBoard, ''),
   getPos(UBoard, LastMove, '').

getNextAvailableBoards(State, Bag):-
   getLastMove(State, LastMove),
   getUBoard(State, UBoard),
   getBoardResult(UBoard, ''),
   \+ getPos(UBoard, LastMove, ''),
   findall(Rez,
           ( positions(All),
             member(Rez, All),
             getPos(UBoard, Rez, '')),
           Bag).



% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :- player_wins(Result, Board),!.
getBoardResult(Board, Result) :- \+ player_wins(_, Board),
    member('', Board), Result = '',!.
getBoardResult(Board, Result) :- \+ player_wins(_, Board),
    \+ member('', Board), Result = r ,!.


% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
buildState(Boards, Prev, [Boards, Prev]).
% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(State,Move) :- getUBoard(State, UBoard),
   getBoardResult(UBoard, ''),
   positions(Positions),
   member(Move, Positions),
   getNextAvailableBoards(State, BoardIndv),
   length(BoardIndv, 1),
   nth0(0, BoardIndv, CoordTabla),
   getBoard(State, CoordTabla, Tabla),
   getPos(Tabla, Move, ''), !.

validMove(State, Move) :- getUBoard(State, UBoard),
   getBoardResult(UBoard, ''),
   positions(Positions),
   \+ member(Move, Positions),
   (PosBoard, PosCell) = Move,
   getNextAvailableBoards(State,AvailableBoards),
   member(PosBoard, AvailableBoards),
   getPos(State, PosBoard,  PosCell, ''), !.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(State, Move, NewState) :-
    validMove(State,Move),
    getBoards(State, Boards),
    getNextPlayer(State, Player),
    getNextAvailableBoards(State, ListaValabile),
    length(ListaValabile,1),
    nth0(0, ListaValabile, SinguraValabila),
    positions(Positions),
    nth0(P, Positions, SinguraValabila),
    nth0(Q, Positions, Move),
    modifyBoards(Boards, P,Q, NewBoards,Player, 0),
    buildState(NewBoards, Move, NewState), !.


makeMove(State, (IndvBoard, Cell), NewState) :-
    validMove(State,(IndvBoard, Cell)),
    getBoards(State, Boards),
    getNextPlayer(State, Player),
    positions(Positions),
    nth0(P, Positions, IndvBoard),
    nth0(Q, Positions, Cell),
    modifyBoards(Boards,P, Q, NewBoards, Player, 0),
    buildState(NewBoards, Cell, NewState), !.
%Pos e un int
modifyBoards([],_,_,[],_,9).
modifyBoards([HOld|TOld], Index, Q, [HNew|TNew], Player, Index):- Index < 9,
    modifyOneBoard(HOld, HNew, Player, Q, 0),
    NewIndex is Index + 1,
    modifyBoards(TOld, Index,Q, TNew, Player, NewIndex),!.
modifyBoards([HOld|TOld], P, Q, [HOld|TNew], Player, Index):- Index < 9,
    \+ Index == P,
    NewIndex is Index + 1,
    modifyBoards(TOld, P,Q, TNew, Player, NewIndex),!.

modifyOneBoard([],[],_,_,_).
modifyOneBoard([_|To],[Hn|Tn], Player, Indx, Indx):- Indx < 9,
    Hn = Player,
    NewIndx is Indx + 1,
    modifyOneBoard(To, Tn, Player, Indx, NewIndx),!.
modifyOneBoard([Ho|To],[Ho|Tn], Player, Q, Indx):- Indx < 9,
    \+ Indx == Q,
    NewIndx is Indx + 1,
    modifyOneBoard(To, Tn, Player, Q, NewIndx),!.
% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(State, NextMove) :-
     positions(Positions),
     getNextAvailableBoards(State, Bag),
     length(Bag, 1),
     nth0(0, Bag, Move),
     getBoard(State, Move, Board),
     nth0(Index, Board, ''),
     nth0(Index, Positions, NextMove),
     !.
dummy_first(State, (P,Q)):-
    positions(Positions),
    getNextAvailableBoards(State,Bag),
    nth0(0,Bag, P),
    getBoard(State, P, Board),
    nth0(Index, Board, ''),
    nth0(Index, Positions, Q).


% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă
% (ultima din lista de poziții disponibile).
dummy_last(State, NextMove) :-
     positions(Positions),
     reverse(Positions,ReversedPositions),
     getNextAvailableBoards(State, Bag),
     length(Bag, 1),
     nth0(0, Bag, Move),
     getBoard(State, Move, Board),
     reverse(ReversedBoard, Board),
     nth0(Index, ReversedBoard, ''),
     nth0(Index, ReversedPositions, NextMove),
     !.
dummy_last(State, (P,Q)):-
    positions(Positions),
    reverse(Positions, ReversedPositions),
    getNextAvailableBoards(State,Bag),
    reverse(ReversedBag, Bag),
    nth0(0,ReversedBag, P),
    getBoard(State, P, Board),
    reverse(ReversedBoard,Board),
    nth0(Index,  ReversedBoard, ''),
    nth0(Index, ReversedPositions, Q).


% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R), !.

winningMove0(Player, Board, Move):- positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''),
    replace(Index, Board, Player, NewBoard)
    ,player_wins(Player, NewBoard), !.
winningMove1(Player, Board, Move):-  positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''),
    nextPlayer(Player, Opponent),
    replace(Index, Board, Opponent, NewBoard)
    ,player_wins(Opponent, NewBoard),!.


winningMove4(Player, Board, Move):-  positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''),
    replace(Index, Board, Player, NewBoard),
    findall(X,
            (member(X,Positions)
             ,nth0(I, Positions, X),
             nth0(I, NewBoard, ''),
             replace(I, NewBoard, Player, NewNewBoard),
             player_wins(Player,NewNewBoard))
           ,L),
    length(L,Len),
    Len >= 1,!.

movePriority(Player, Board, Move, 0) :-
        winningMove0(Player, Board, Move),!.

movePriority(Player, Board, Move, 1) :- winningMove1(Player, Board, Move),!.

movePriority(_, Board, Move, 2) :- empty_board(Board), member(Move,[nw, ne, sw, se]),!.

movePriority(Player, Board, Move, Priority) :- \+ member(Player, Board),
    nextPlayer(Player, Opponent),
    nth0(4,Board,Opponent),
    positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''),
    member(Move,[nw, ne, sw, se]),
    Priority = 3,!.

movePriority(Player, Board, Move, Priority) :- \+ member(Player, Board),
    nextPlayer(Player, Opponent),
    \+ nth0(4,Board,Opponent),
     positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''),
    Move == c,
    Priority = 3,!.
movePriority(Player, Board, Move, Priority) :-  winningMove4(Player, Board, Move),Priority = 4, !.

movePriority(_, Board, Move, 5) :- positions(Positions),
    nth0(Index, Positions, Move),
     nth0(Index,Board, ''),
    Move == nw ; Move == ne ; Move == sw ; Move == se,!.
movePriority(_, Board, Move, 6):-  positions(Positions),
    nth0(Index, Positions, Move),
    nth0(Index,Board, ''), !.
% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
bestIndividualMoves(Player, Board, Moves) :-
    positions(Positions),
    findall((P, M),
            ( member(M, Positions),
              nth0(Index, Positions, M),
              nth0(Index,Board, ''),
              movePriority(Player, Board, M, P)),
            L),
    sortMoves(L, Moves).


% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.
narrowGreedy(State, Move) :-
    getNextAvailableBoards(State, PosBoards),
    length(PosBoards,Len),
    Len == 1,
    getNextPlayer(State,Player),
    nth0(0, PosBoards, Pos),
    getBoard(State, Pos, Board),
    bestIndividualMoves(Player,Board, Moves),
    nth0(0, Moves, Move),!.

narrowGreedy(State, Move) :- getNextAvailableBoards(State, PosBoards),    length(PosBoards,Len),
    Len > 1,
    getNextPlayer(State,Player),
    getUBoard(State,UBoard),
    bestIndividualMoves(Player,UBoard, UMoves),
    nth0(0,UMoves, P),
    getBoard(State, P, Board),
    bestIndividualMoves(Player,Board,Moves),
    nth0(0, Moves, Pindv),
    Move = (P, Pindv),!.




% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves(_, _) :- false.

% greedy/2
% greedy(+State, -Move)
greedy(_, _) :- false.
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(_, _) :- false.
