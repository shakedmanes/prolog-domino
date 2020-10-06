/*
 * Author: Shaked Manes
 * Purpose: Game Project for MAMAN 17
 * Date: XX/10/2020
 *
 * This program was developed using SWI-Prolog, so it depends
 * on the SWI-Prolog Prolog implementation standards.
 */

:- dynamic(curr_game_state/1).
:- dynamic(game_board/1).
:- dynamic(player_one_hand/1).
:- dynamic(player_two_hand/1).
:- dynamic(curr_player_turn/1).
:- dynamic(last_tile_right/1).
:- dynamic(last_tile_left/1).

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


game_state_attacher(_MENU, run_menu_selection).

/**
 * Game roles predicates
 */

/** Bones tiles of the game **/
bone(0, 0).

bone(0, 1).
bone(1, 1).

bone(0, 2).
bone(1, 2).
bone(2, 2).

bone(0, 3).
bone(1, 3).
bone(2, 3).
bone(3, 3).

bone(0, 4).
bone(1, 4).
bone(2, 4).
bone(3, 4).
bone(4, 4).

bone(0, 5).
bone(1, 5).
bone(2, 5).
bone(3, 5).
bone(4, 5).
bone(5, 5).

bone(0, 6).
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
    cut_wrapper(print_open_game_message),
    cut_wrapper(open_game_menu),
    cleanup,
    seen,
    told.

open_game_menu:-
    cut_wrapper(print_message('')),
    cut_wrapper(print_main_options),
    cut_wrapper(get_user_selection(1-4, Selection)),
    cut_wrapper(run_menu_selection(Selection)).

run_menu_selection(Selection):-
    (
        (
            Selection == 1,
            start_new_game,
            open_game_menu
        )
        ;
        (
            Selection == 2,
            print_instructions,
            open_game_menu
        )
        ;
        (
            Selection == 3,
            enter_settings,
            open_game_menu
        )
        ;
        (
            Selection == 4,
            exit_game
        )
    ).

start_new_game:-
    print_message('Starting new game...'),
    sleep(1),
    cleanup,
    print_message('Shuffling bone tiles...'),
    sleep(1),
    generate_boneyard(GeneratedBoneyard),
    pick_tiles_from_boneyard(
        7,
        GeneratedBoneyard,
        UpdatedBoneyard,
        PlayerOnePickedTiles
    ),
    pick_tiles_from_boneyard(
        7,
        UpdatedBoneyard,
        _,
        PlayerTwoPickedTiles
    ),
    print_message('Drawing bones for each player...'),
    sleep(1),
    print_message('Player 1 Bones:'),
    print_message(PlayerOnePickedTiles),
    print_message('Plaer 2 Bones:'),
    print_message(PlayerTwoPickedTiles).


% When trying to pick from empty boneyard, return empty updated boneyard
% and empty picked tiles.
pick_tiles_from_boneyard(_, [], [], []):-
    !.

% Getting one bone from boneyard, should return that bone in picked
% tiles and update the boneyard for removing that bone.
pick_tiles_from_boneyard(1, [BoneTile | RestBoneyard], RestBoneyard, [BoneTile]):-
    !.

% For each bone, remove it from the boneyard, and add it to the
% picked tiles.
% Continue this process until reaching the last bone tile to collect.
pick_tiles_from_boneyard(
    NumTiles,
    [BoneTile | RestBoneyard],
    UpdatedBoneyard,
    [BoneTile | PickedTiles]
):-
    NewNumTiles is NumTiles - 1,
    pick_tiles_from_boneyard(
        NewNumTiles,
        RestBoneyard,
        UpdatedBoneyard,
        PickedTiles
    ),
    !.

board_is_empty(Board-Board, empty):- !.
board_is_empty([_ | RestBoard]-RestBoard, noempty):- !.

% append_tile_on_board(T1-T1, Tile, _, [Tile | T1]-T1):- !.
% For this to work, board MUST be declared empty as [T1]-T1.
append_tile_on_board(
    LeftSideBoard-RestBoard,
    Tile,
    left,
    [Tile | LeftSideBoard]-RestBoard
):-
    !,
    retractall(last_tile_left(_)),
    assert(last_tile_left(Tile)).

append_tile_on_board(
    LeftSideBoard-[Tile | RestBoard],
    Tile,
    right,
    LeftSideBoard-RestBoard
):-
    !,
    retractall(last_tile_right(_)),
    assert(last_tile_right(Tile)).

get_last_tiles(LeftTile, RightTile):-
    last_tile_left(LeftTile),
    last_tile_right(RightTile).

enter_settings:-
    print_message('Enter settings...').

exit_game:-
    print_message('Bye, see you next time!').

set_curr_game_state(GameState):-
    retractall(curr_game_state(_)),
    assert(curr_game_state(GameState)).

cleanup:-
    retractall(curr_game_state(_)),
    retractall(game_board(_)),
    retractall(player_one_hand(_)),
    retractall(player_two_hand(_)),
    retractall(curr_player_turn(_)),
    retractall(last_tile_right(_)),
    retractall(last_tile_left(_)).

get_user_selection(StartRange-EndRange, Selection):-
    print_message('Your selection: '),
    get_user_input(UserInput),
    !,
    print_message(''),
    validate_user_selection(UserInput, StartRange-EndRange, Validity),
    (
        (
            Validity == valid,
            !,
            Selection is UserInput
        )
        ;
        (
            Validity == novalid,
            !,
            print_message('Invalid selection was given, try again.'),
            get_user_selection(StartRange-EndRange, Selection)
        )
    ).

/** Game Helpers **/
validate_user_selection(Selection, From-To, valid):-
    integer(Selection),
    Selection >= From,
    Selection =< To.

validate_user_selection(_, _, novalid).

get_user_input(UserInput):-
	get(Char),
	keep_get_user_input(RestChars),
	append([Char], RestChars, CharacterList),
	name(UserInput, CharacterList).

% recursive helper parser routine: parse_rest_of_line(-Result)
keep_get_user_input(Chars):-
    get0(Char),
    (
        % Char is new line ('\n' = 10, means we at end of user input)
        (
             Char = 10,
             !,
             Chars = []
        )
        ;
        % Char is space (' ' = 32, means rest user input is useless)
        (
             Char = 32,
             !,
             Chars = [],
             keep_get_useless_user_input
        )
        ;
        % Char is ok, continue getting user input
        (
             Chars = [Char | Rest],
             keep_get_user_input(Rest)
        )
    ).

% Continue to read until the end
keep_get_useless_user_input:-
    get0(UselessChar),

    % Continue getting useless characters until reaching end of input
    % at new line ('\n' = 10)
    (
        (UselessChar = 10, !)
        ;
        (keep_get_useless_user_input)
    ).

print_instructions:-
    cut_wrapper(print_message('##### Game Instructions #####')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('There are 28 bone tiles, which contains numbers from 0 to 6.')),
    cut_wrapper(print_message('Each bone tile looks like this: [X|Y], which X and Y represents the numbers on the bone tile.')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('The game flow runs like this:')),
    cut_wrapper(print_message('Each player gets 7 bone tiles, other bone tiles remains at the side and used as boneyard (Will be explained later).')),
    cut_wrapper(print_message('The player which have the highest bone tile which contains double (Both X and Y equal) start his turn.')),
    cut_wrapper(print_message('If both players does not have double bone tile, the player with the highest bone tile (X+Y result of the bone tile) starts his turn.')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('Each player in his turn, put one bone tile at the game board, in a way the bone tiles touch end to end (the touching ends must match: i.e., one’s touch one’s, two’s touch two’s, etc.)')),
    cut_wrapper(print_message('When a player cannot put bone tile on the board, the player need to draw one bone tile from the boneyard and the turn passed to the other player')),
    cut_wrapper(print_message('When a player cannot put bone tile on the board and the boneyard is empty, the turn passed to the other player')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('The winner is the player which done placing all his bone tiles on the game board')),
    cut_wrapper(print_message('If both players cannot place bone tiles on the board, the winner is the player which have the lower bone tiles sum (Sum of all X and Y of all bone tiles)')),
    cut_wrapper(print_message('')),
    cut_wrapper(print_message('##############################')),
    cut_wrapper(print_message('')).

print_main_options:-
    cut_wrapper(print_message('Please, select one of the following options to proceed:')),
    cut_wrapper(print_message('1. New Game')),
    cut_wrapper(print_message('2. Instructions')),
    cut_wrapper(print_message('3. Settings')),
    cut_wrapper(print_message('4. Exit')),
    cut_wrapper(print_message('')).

print_open_game_message:-
    cut_wrapper(print_message('Welcome to Prolog-Domino Game!')),
    cut_wrapper(print_message('A Domino game implemented in Prolog, which is fun and challenging.')).


/** UI Predicates **/
print_message(Message):-
    write(Message),
    nl.

get_user_selection(Selection):-
    get(Selection).

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



