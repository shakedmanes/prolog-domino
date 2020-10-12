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
:- dynamic(player_identity/2).

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
 * remove_element_from_list(Element, GivenList, ReturnedList):-
 *   Removes Element given from the GivenList, and return the GivenList
 *   without the Element in ReturnedList.
 *
 *   INPUT:
 *     Element - Given element to remove from the GivenList.
 *     GivenList - A list to search for the Element given.
 *   OUTPUT:
 *     ReturnedList - The GivenList without the Element.
 */

% Iterate over the list, when finding element which is not equal to the
% given element, continue searching.
remove_element_from_list(Element, [NotElement | RestList1], [NotElement | RestList2]):-
    Element \= NotElement,
    !,
    remove_element_from_list(Element, RestList1, RestList2).

% When finding the element in the list, remove it and keep gather all
% rest elements.
remove_element_from_list(Element, [Element | RestList1], RestList2):-
    !,
    remove_element_from_list(Element, RestList1, RestList2).

% Empty list will return always empty list.
remove_element_from_list(_, [], []).


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

/** Displayable bone tiles **/
displayable_bone(bone(0, 0), "\uD83C\uDC31").
displayable_bone(bone(0, 1), "\uD83C\uDC32").
displayable_bone(bone(0, 2), "\uD83C\uDC33").
displayable_bone(bone(0, 3), "\uD83C\uDC34").
displayable_bone(bone(0, 4), "\uD83C\uDC35").
displayable_bone(bone(0, 5), "\uD83C\uDC36").
displayable_bone(bone(0, 6), "\uD83C\uDC37").

displayable_bone(bone(1, 0), "\uD83C\uDC38").
displayable_bone(bone(1, 1), "\uD83C\uDC39").
displayable_bone(bone(1, 2), "\uD83C\uDC3A").
displayable_bone(bone(1, 3), "\uD83C\uDC3B").
displayable_bone(bone(1, 4), "\uD83C\uDC3C").
displayable_bone(bone(1, 5), "\uD83C\uDC3D").
displayable_bone(bone(1, 6), "\uD83C\uDC3E").


displayable_bone(bone(2, 0), "\uD83C\uDC3F").
displayable_bone(bone(2, 1), "\uD83C\uDC40").
displayable_bone(bone(2, 2), "\uD83C\uDC41").
displayable_bone(bone(2, 3), "\uD83C\uDC42").
displayable_bone(bone(2, 4), "\uD83C\uDC43").
displayable_bone(bone(2, 5), "\uD83C\uDC44").
displayable_bone(bone(2, 6), "\uD83C\uDC45").

displayable_bone(bone(3, 0), "\uD83C\uDC46").
displayable_bone(bone(3, 1), "\uD83C\uDC47").
displayable_bone(bone(3, 2), "\uD83C\uDC48").
displayable_bone(bone(3, 3), "\uD83C\uDC49").
displayable_bone(bone(3, 4), "\uD83C\uDC4A").
displayable_bone(bone(3, 5), "\uD83C\uDC4B").
displayable_bone(bone(3, 6), "\uD83C\uDC4C").

displayable_bone(bone(4, 0), "\uD83C\uDC4D").
displayable_bone(bone(4, 1), "\uD83C\uDC4E").
displayable_bone(bone(4, 2), "\uD83C\uDC4F").
displayable_bone(bone(4, 3), "\uD83C\uDC50").
displayable_bone(bone(4, 4), "\uD83C\uDC51").
displayable_bone(bone(4, 5), "\uD83C\uDC52").
displayable_bone(bone(4, 6), "\uD83C\uDC53").

displayable_bone(bone(5, 0), "\uD83C\uDC54").
displayable_bone(bone(5, 1), "\uD83C\uDC55").
displayable_bone(bone(5, 2), "\uD83C\uDC56").
displayable_bone(bone(5, 3), "\uD83C\uDC57").
displayable_bone(bone(5, 4), "\uD83C\uDC58").
displayable_bone(bone(5, 5), "\uD83C\uDC59").
displayable_bone(bone(5, 6), "\uD83C\uDC5A").

displayable_bone(bone(6, 0), "\uD83C\uDC5B").
displayable_bone(bone(6, 1), "\uD83C\uDC5C").
displayable_bone(bone(6, 2), "\uD83C\uDC5D").
displayable_bone(bone(6, 3), "\uD83C\uDC5E").
displayable_bone(bone(6, 4), "\uD83C\uDC5F").
displayable_bone(bone(6, 5), "\uD83C\uDC60").
displayable_bone(bone(6, 6), "\uD83C\uDC61").

displayable_hand_bone(hide_bone, "\uD83C\uDC62").

displayable_hand_bone(bone(0, 0), "\uD83C\uDC63").
displayable_hand_bone(bone(0, 1), "\uD83C\uDC64").
displayable_hand_bone(bone(0, 2), "\uD83C\uDC65").
displayable_hand_bone(bone(0, 3), "\uD83C\uDC66").
displayable_hand_bone(bone(0, 4), "\uD83C\uDC67").
displayable_hand_bone(bone(0, 5), "\uD83C\uDC68").
displayable_hand_bone(bone(0, 6), "\uD83C\uDC69").

displayable_hand_bone(bone(1, 0), "\uD83C\uDC6A").
displayable_hand_bone(bone(1, 1), "\uD83C\uDC6B").
displayable_hand_bone(bone(1, 2), "\uD83C\uDC6C").
displayable_hand_bone(bone(1, 3), "\uD83C\uDC6D").
displayable_hand_bone(bone(1, 4), "\uD83C\uDC6E").
displayable_hand_bone(bone(1, 5), "\uD83C\uDC6F").
displayable_hand_bone(bone(1, 6), "\uD83C\uDC70").

displayable_hand_bone(bone(2, 0), "\uD83C\uDC71").
displayable_hand_bone(bone(2, 1), "\uD83C\uDC72").
displayable_hand_bone(bone(2, 2), "\uD83C\uDC73").
displayable_hand_bone(bone(2, 3), "\uD83C\uDC74").
displayable_hand_bone(bone(2, 4), "\uD83C\uDC75").
displayable_hand_bone(bone(2, 5), "\uD83C\uDC76").
displayable_hand_bone(bone(2, 6), "\uD83C\uDC77").

displayable_hand_bone(bone(3, 0), "\uD83C\uDC78").
displayable_hand_bone(bone(3, 1), "\uD83C\uDC79").
displayable_hand_bone(bone(3, 2), "\uD83C\uDC7A").
displayable_hand_bone(bone(3, 3), "\uD83C\uDC7B").
displayable_hand_bone(bone(3, 4), "\uD83C\uDC7C").
displayable_hand_bone(bone(3, 5), "\uD83C\uDC7D").
displayable_hand_bone(bone(3, 6), "\uD83C\uDC7E").

displayable_hand_bone(bone(4, 0), "\uD83C\uDC7F").
displayable_hand_bone(bone(4, 1), "\uD83C\uDC80").
displayable_hand_bone(bone(4, 2), "\uD83C\uDC81").
displayable_hand_bone(bone(4, 3), "\uD83C\uDC82").
displayable_hand_bone(bone(4, 4), "\uD83C\uDC83").
displayable_hand_bone(bone(4, 5), "\uD83C\uDC84").
displayable_hand_bone(bone(4, 6), "\uD83C\uDC85").

displayable_hand_bone(bone(5, 0), "\uD83C\uDC86").
displayable_hand_bone(bone(5, 1), "\uD83C\uDC87").
displayable_hand_bone(bone(5, 2), "\uD83C\uDC88").
displayable_hand_bone(bone(5, 3), "\uD83C\uDC89").
displayable_hand_bone(bone(5, 4), "\uD83C\uDC8A").
displayable_hand_bone(bone(5, 5), "\uD83C\uDC8B").
displayable_hand_bone(bone(5, 6), "\uD83C\uDC8C").

displayable_hand_bone(bone(6, 0), "\uD83C\uDC8D").
displayable_hand_bone(bone(6, 1), "\uD83C\uDC8E").
displayable_hand_bone(bone(6, 2), "\uD83C\uDC8F").
displayable_hand_bone(bone(6, 3), "\uD83C\uDC90").
displayable_hand_bone(bone(6, 4), "\uD83C\uDC91").
displayable_hand_bone(bone(6, 5), "\uD83C\uDC92").
displayable_hand_bone(bone(6, 6), "\uD83C\uDC93").

/** Displaying players names **/
displayable_player(player_one, 'Player one').
displayable_player(player_two, 'Player two').

displayable_player_type(user_player, 'You').
displayable_player_type(computer_random, 'Computer Random Bot').
displayable_player_type(computer_statistical, 'Computer Statistical Bot').


% Reversing bone direction utility
reverse_bone_dir(bone(LeftVal, RightVal), bone(RightVal, LeftVal)).

% Generates random boneyard for the game
generate_boneyard(RandomBoneyard):-
    bagof(bone(X,Y), bone(X, Y), BoneList),
    random_permutation(BoneList, RandomBoneyard).


start:-
    see(user),
    tell(user),
    cut_wrapper(print_open_game_message),
    cut_wrapper(set_default_settings),
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
    print_player_full_hand(PlayerOnePickedTiles),
    print_message(''),
    print_message('Player 2 Bones:'),
    print_computer_full_hand(PlayerTwoPickedTiles),
    print_message(''),
    assert(player_one_hand(PlayerOnePickedTiles)),
    assert(player_two_hand(PlayerTwoPickedTiles)),
    sleep(1),
    determine_starter_player(
        PlayerOnePickedTiles, PlayerTwoPickedTiles, PlayerStarter, MaxTile
    ),
    play_automatic_starter_player(PlayerStarter, MaxTile),
    start_game_loop.

start_game_loop.


% Play automatically for starter player.
% Basically put the tile on the game board and move the turn to the next
% player.
play_automatic_starter_player(PlayerStarter, MaxTile):-
    print_message(''),
    print_message_without_nl('Playing automatically for starter player: '),
    print_message(PlayerStarter),
    sleep(1),
    print_message_without_nl('Player '),
    print_message_without_nl(PlayerStarter),
    print_message_without_nl(' Put the tile '),
    print_board_tile(MaxTile),
    print_message_without_nl(' on the board'),
    print_message(''),
    assert(game_board([MaxTile | T1]-T1)),
    remove_tile_from_player_hand(PlayerStarter, MaxTile).

% Removes tile from a given player hand.
remove_tile_from_player_hand(Player, Tile):-
    (
	(
            Player == player_one,
            player_one_hand(PlayerOneHand),
            remove_element_from_list(Tile, PlayerOneHand, NewPlayerOneHand),
            retractall(player_one_hand(_)),
            assert(player_one_hand(NewPlayerOneHand))
        )
	;
	(
            player_two_hand(PlayerTwoHand),
            remove_element_from_list(Tile, PlayerTwoHand, NewPlayerTwoHand),
            retractall(player_two_hand(_)),
            assert(player_two_hand(NewPlayerTwoHand))
        )
    ).

get_appendable_tiles(Tiles, AppendableTiles):-
    setof(
        possible_moves(Tile, Side, Reversed),
        (
            member(Tile, Tiles),
            check_appendable_tile(Tile, Side, Reversed, Validity),
            Validity \== novalid
        ),
        AppendableTiles
    ).

check_appendable_tile(Tile, Side, Reversed, Validity):-
    get_last_tiles(bone(LeftTileLeftValue, _), bone(_, RightTileRightValue)),
    Tile = bone(CheckLeftValue, CheckRightValue),
    (
	(
            CheckLeftValue =:= CheckRightValue,
            !,
            Reversed = no,
            (
		(
                    Side = right,
                    RightTileRightValue =:= CheckRightValue,
                    Validity = valid
                );
		(
                    Side = left,
                    LeftTileLeftValue =:= CheckLeftValue,
                    Validity = valid
                );
		(
                    Side = uninitialized,
                    Validity = novalid
                )
            )
        );
	(
            (
                Side = right,
                (
                    (
                        RightTileRightValue =:= CheckLeftValue,
                        Reversed = no,
                        Validity = valid
                    );
                    (
			RightTileRightValue =:= CheckRightValue,
                        Reversed = yes,
                        Validity = valid
                    )
                )
            );
            (
		Side = left,
		(
                    (
			LeftTileLeftValue =:= CheckRightValue,
                        Reversed = no,
                        Validity = valid
                    );
                    (
			LeftTileLeftValue =:= CheckLeftValue,
                        Reversed = yes,
                        Validity = valid
                    )
                )
            );
            (
		Side = uninitialized,
                Reversed = uninitialized,
                Validity = novalid
            )
	)
    ).

determine_starter_player(
    PlayerOnePickedTiles,
    PlayerTwoPickedTiles,
    PlayerStarter,
    MaxTile
):-
    print_message('Determining first turn...'),
    sleep(1),
    get_starter_player_and_tile(
        PlayerOnePickedTiles,
        PlayerTwoPickedTiles,
        MaxTile,
        PlayerStarter
    ),
    sleep(1),
    print_message('Max tile found: '),
    print_player_tile(MaxTile),
    print_message(''),
    sleep(1),
    print_message('The player which starts the game is: '),
    print_message(PlayerStarter),
    set_current_turn(PlayerStarter).

set_next_turn:-
    curr_player_turn(CurrentPlayer),
    (
        (
            CurrentPlayer == player_one,
            set_current_turn(player_two)
        );
        set_current_turn(player_two)
    ).

% Sets the PlayerID as the current turn player
set_current_turn(PlayerID):-
    retractall(curr_player_turn(_)),
    assert(curr_player_turn(PlayerID)).

% Sets the Player type for a given Player (player_one / player_two)
set_player_identity(Player, PlayerType):-
    retractall(player_identity(Player, _)),
    assert(player_identity(Player, PlayerType)).

% Sets the default settings for the game to start
set_default_settings:-
    set_player_identity(player_one, user_player),
    set_player_identity(player_two, computer_random).

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
    retractall(game_board(_)),
    assert(last_tile_left(Tile)),
    assert(game_board([Tile | LeftSideBoard]-RestBoard)).

append_tile_on_board(
    LeftSideBoard-[Tile | RestBoard],
    Tile,
    right,
    LeftSideBoard-RestBoard
):-
    !,
    retractall(last_tile_right(_)),
    retractall(game_board(_)),
    assert(last_tile_right(Tile)),
    assert(game_board(LeftSideBoard-RestBoard)).

get_last_tiles(LeftTile, RightTile):-
    last_tile_left(LeftTile),
    last_tile_right(RightTile).

% Estimates the bigger tile between the two tiles given.
% The predicate unify only if the first tile is the maximum between the
% two.
estimate_bigger_tile(
    bone(BiggerLeft, BiggerRight),
    bone(LowerLeft, LowerRight)
):-
    (
	(
            BiggerLeft == BiggerRight,
            (
		(
                    LowerLeft == LowerRight,
                    BiggerLeft >= LowerLeft
                );
		LowerLeft \== LowerRight
            )
        )
	;
	(
            BiggerLeft \== BiggerRight,
            LowerLeft \== LowerRight,
            BiggerSum is BiggerLeft + BiggerRight,
            LowerSum is LowerRight + LowerLeft,
            BiggerSum >= LowerSum
        )
    ).

% Getting the maximum of player hand (list of tile), using
% estimate_bigger_tile which unify if the first tile is bigger than the
% other.
get_max_tile_of_player([Tile], Tile):- !.
get_max_tile_of_player([Tile | RestPlayerHand], MaxTile):-
    get_max_tile_of_player(RestPlayerHand, RestMaxTile),
    !,
    (
	(
            estimate_bigger_tile(Tile, RestMaxTile),
            MaxTile = Tile
        )
	;
	(
	    MaxTile = RestMaxTile
        )
    ).

% Gets the max tile between the two players and the player which
% should start to play (The player which has the max tile in his hand,
% should place it in the game board).
% Starter values are player_one/player_two.
get_starter_player_and_tile(PlayerOneTiles, PlayerTwoTiles, MaxTile, Starter):-
    get_max_tile_of_player(PlayerOneTiles, PlayerOneMaxTile),
    get_max_tile_of_player(PlayerTwoTiles, PlayerTwoMaxTile),
    (
	(
	    estimate_bigger_tile(PlayerOneMaxTile, PlayerTwoMaxTile),
            MaxTile = PlayerOneMaxTile,
            Starter = player_one
        )
	;
	(
            MaxTile = PlayerTwoMaxTile,
            Starter = player_two
        )
    ).

enter_settings:-
    print_settings_screen,
    control_settings.

control_settings:-
    print_settings_main_choices,
    get_user_selection(1-3, Selection),
    run_settings_main_selection(Selection).

run_settings_main_selection(Selection):-
    (
        (
            Selection == 1,
            enter_player_selection(player_one),
            enter_settings
        )
        ;
        (
            Selection == 2,
            enter_player_selection(player_two),
            enter_settings
        )
        ;
        (
            print_message('Exiting Settings'),
            print_message('')
        )
    ).

enter_player_selection(Player):-
    print_message_without_nl('Please, select one of the below options for '),
    displayable_player(Player, DisplayPlayer),
    print_message_without_nl(DisplayPlayer),
    print_message(':'),
    print_player_options,
    get_user_selection(1-4, Selection),
    control_player_selection(Player, Selection).

control_player_selection(Player, Selection):-
    (
        (
            Selection == 1,
            set_player_identity(Player, user_player)
        )
        ;
        (
            Selection == 2,
            set_player_identity(Player, computer_random)
        )
        ;
        (
            Selection == 3,
            set_player_identity(Player, computer_statistical)
        )
        ;
        (
            print_message('Exit player selection.'),
            print_message('')
        )
    ).

print_player_options:-
    print_message(''),
    print_message_without_nl('1. '),
    displayable_player_type(user_player, DisplayUser),
    print_message(DisplayUser),
    print_message_without_nl('2. '),
    displayable_player_type(computer_random, DisplayCompRand),
    print_message(DisplayCompRand),
    print_message_without_nl('3. '),
    displayable_player_type(computer_statistical, DisplayCompStats),
    print_message(DisplayCompStats),
    print_message('4. Exit Selection'),
    print_message('').

print_settings_main_choices:-
    print_message(''),
    print_message('Please, select one of the following options to proceed:'),
    print_message('1. Change Player One Type'),
    print_message('2. Change Player Two Type'),
    print_message('3. Exit'),
    print_message('').

print_settings_screen:-
    print_message('######## Settings Menu ########'),
    print_message(''),
    print_message('Here you can select which players will play the game.'),
    print_message(''),
    print_message('Currently settings:'),
    print_message(''),
    print_message_without_nl('Player one: '),
    player_identity(player_one, PlayerOneIdentity),
    displayable_player_type(PlayerOneIdentity, DisplayPlayerOne),
    print_message(DisplayPlayerOne),
    print_message_without_nl('Player two: '),
    player_identity(player_two, PlayerTwoIdentity),
    displayable_player_type(PlayerTwoIdentity, DisplayPlayerTwo),
    print_message(DisplayPlayerTwo),
    print_message(''),
    print_message('############################').



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
    retractall(last_tile_left(_)),
    retractall(player_identity(_)).

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


print_computer_full_hand(Tiles):-
    forall(
        member(_, Tiles),
        print_hide_tile
    ).


print_player_full_hand(Tiles):-
    forall(
        member(Tile, Tiles),
        print_player_tile(Tile)
    ).

print_player_tile(Tile):-
    displayable_hand_bone(Tile, UnicodeValue),
    print_unicode_value(UnicodeValue).


print_board_tile(Tile):-
    displayable_bone(Tile, UnicodeValue),
    print_unicode_value(UnicodeValue).


print_hide_tile:-
    displayable_hand_bone(hide_bone, UnicodeValue),
    print_unicode_value(UnicodeValue).


print_unicode_value(UnicodeValue):-
    atom_string(X, UnicodeValue),
    write(X).


/** UI Predicates **/
print_message(Message):-
    write(Message),
    nl.

print_message_without_nl(Message):-
    write(Message).

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



