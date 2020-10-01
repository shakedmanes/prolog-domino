/*
 * Author: Shaked Manes
 * Purpose: Game Project for MAMAN 17
 * Date: XX/10/2020
 *
 */

/** Utilities **/

/**
 * cut_wrapper(Predicate):-
 *   Wrap Predicate with cut to run the predicate only once without
 *   backtracking.
 *
 *   INPUT:
 *     P - Predicate to run with cut.
 */
cut_wrapper(Predicate):-
    call(Predicate),
    !.
cut_wrapper(_).


/**
 * Game roles predicates
 */

/** Bones tiles of the game **/
bone(blank, blank).
bone(blank, 1).
bone(1, 1).

bone(blank, 2).
bone(1, 2).
bone(2, 2).

bone(blank, 3).
bone(1, 3).
bone(2, 3).
bone(3, 3).

bone(blank, 4).
bone(1, 4).
bone(2, 4).
bone(3, 4).
bone(4, 4).

bone(blank, 5).
bone(1, 5).
bone(2, 5).
bone(3, 5).
bone(4, 5).
bone(5, 5).

bone(blank, 6).
bone(1, 6).
bone(2, 6).
bone(3, 6).
bone(4, 6).
bone(5, 6).
bone(6, 6).

% Reversing bone direction utility
reverse_bone_dir(bone(LeftVal, RightVal), bone(RightVal, LeftVal)).

% Generates random boneyard for the game
generate_boneyard(RandomBoneyard):-
    bagof((X,Y), bone(X, Y), BoneList),
    random_permutation(BoneList, RandomBoneyard).


start:-
    see(user),
    tell(user),
    cut_wrapper(print_message('Welcome to Prolog-Domino Game!')),
    cut_wrapper(print_message('A Domino game implemented in Prolog, which is fun and challenging.')),
    cut_wrapper(print_message('Please, select one of the following options to proceed:')),
    cut_wrapper(print_message('1. New Game')),
    cut_wrapper(print_message('2. Instructions')),
    cut_wrapper(print_message('3. Settings')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('Your selection: ')),
    cut_wrapper(get_user_selection(selection)),
    seen,
    told.

/** UI Predicates **/
print_message(Message):-
    write(Message),
    nl.

get_user_selection(selection):-
    get(selection).

% NOTE: Maybe I'll use it, commented for now.
/**

/**
 * Alpha Beta algorithm Implementation
 */
alphabeta(Pos, Alpha, Beta, GoodPos, Val):-
    moves(Pos, PosList), !,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val);
    staticval(Pos, Val). % Static value of Pos

boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal):-
    alphabeta(Pos, Alpha, Beta, _, Val),
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

% No other candidate.
goodenough([], _, _, Pos, Val, Pos, Val):-
    !.

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val):-
    min_to_move(Pos), Val > Beta, !;   % Maximizer attained upper bound
    max_to_move(Pos), Val < Alpha, !.  % Minimizer attained lower bound

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal):-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta):-
    min_to_move(Pos), Val > Alpha, !.  % Maximizer increased upper bound

newbounds(Alpha, Beta, Pos, Val, Alpha, Val):-
    max_to_move(Pos), Val < Beta, !.   % Minimizer decreased upper bound

% Otherwise bounds unchanged
newbounds(Alpha, Beta, _, _, Alpha, Beta).

% Pos better than Pos1
betterof(Pos, Val, Pos1, Val1, Pos, Val):-
    min_to_move(Pos), Val > Val1, !;
    max_to_move(Pos), Val < Val1, !.

% Otherwise Pos1 better
betterof(_, _, Pos1, Val1, Pos1, Val1).

**/



