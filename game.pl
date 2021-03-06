/*
 * Author: Shaked Manes
 * Purpose: Game Project for MAMAN 17
 * Date: 31/10/2020
 *
 * This program was developed using SWI-Prolog, so it depends
 * on the SWI-Prolog Prolog implementation standards.
 *
 * Also note that this programs requires SWI-Prolog font size of 36
 * to run properly.
 */

/** Modules **/
:- use_module(library(random)).
:- use_module('game.config').
:- use_module('game.utils').

/** Dynamic predicates **/
:- dynamic(game_state/1).
:- dynamic(game_board/1).
:- dynamic(game_boneyard/1).
:- dynamic(game_boneyard_count/1).
:- dynamic(game_result/1).
:- dynamic(curr_player_turn/1).
:- dynamic(last_tile_right/1).
:- dynamic(last_tile_left/1).
:- dynamic(player_identity/2).
:- dynamic(player_hand/2).
:- dynamic(player_hand_count/2).
:- dynamic(player_hand_weight/2).
:- dynamic(player_decision_matrix/2).
:- dynamic(computer_tiles_visible/1).

/** Game Flow & Logic Predicates **/

% Start game predicate, used for starting the game.
start:-
    see(user),
    tell(user),
    cleanup,
    cut_wrapper(print_open_game_message),
    cut_wrapper(set_default_settings),
    cut_wrapper(open_game_menu),
    seen,
    told.

% Open game menu, which shows the main options for the game.
open_game_menu:-
    cut_wrapper(print_message('')),
    cut_wrapper(print_main_options),
    cut_wrapper(get_user_selection(1-4, Selection)),
    cut_wrapper(run_menu_selection(Selection)).

% Perform game main menu option selection.
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

% Enter the settings screen and control the selection made by the user.
enter_settings:-
    print_settings_screen,
    control_settings.

% Control the settings options given for the user in the main section
% of the settings screen.
control_settings:-
    print_settings_main_choices,
    get_user_selection(1-4, Selection),
    run_settings_main_selection(Selection).

% Run the settings selection operation by the selected option by the
% user.
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
            Selection == 3,
            toggle_computer_tiles_visible,
            enter_settings
        )
        ;
        (
            print_message('Exiting Settings'),
            print_message('')
        )
    ).

% Select a particular player type for a given player by the user.
% Prints all the player types and set the player type selected by the
% user.
enter_player_selection(Player):-
    print_message_without_nl('Please, select one of the below options for '),
    displayable_player(Player, DisplayPlayer),
    print_message_without_nl(DisplayPlayer),
    print_message(':'),
    print_player_options,
    get_user_selection(1-5, Selection),
    control_player_selection(Player, Selection).

% Control the selection option for a player selected by the user for a
% given player type.
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
            set_player_identity(Player, computer_greedy)
        )
        ;
        (
            Selection == 4,
            set_player_identity(Player, computer_statistical)
        )
        ;
        (
            print_message('Exit player selection.'),
            print_message('')
        )
    ).

% Exits the game by printing exit game and perform cleanup.
exit_game:-
    cleanup,
    print_exit_game.

% Starts a brand new game between 2 players.
start_new_game:-
    cleanup_game_states,
    set_game_state(in_game),
    prepare_player(player_one),
    prepare_player(player_two),
    print_message('Starting new game...'),
    sleep(2),
    print_message('Shuffling bone tiles...'),
    sleep(2),
    generate_boneyard(GeneratedBoneyard),
    set_game_boneyard(GeneratedBoneyard),
    set_game_boneyard_count(28),
    pick_tiles_from_boneyard(7, PlayerOnePickedTiles),
    pick_tiles_from_boneyard(7, PlayerTwoPickedTiles),
    print_message('Drawing bones for each player...'),
    sleep(2),
    print_message('Player 1 Bones:'),
    print_player_full_hand(player_one, PlayerOnePickedTiles),
    print_message(''),
    print_message('Player 2 Bones:'),
    print_player_full_hand(player_two, PlayerTwoPickedTiles),
    print_message(''),
    assert(player_hand(player_one, PlayerOnePickedTiles)),
    assert(player_hand(player_two, PlayerTwoPickedTiles)),
    assert(player_hand_count(player_one, 7)),
    assert(player_hand_count(player_two, 7)),
    calculate_and_set_player_hand_weight(player_one, PlayerOnePickedTiles),
    calculate_and_set_player_hand_weight(player_two, PlayerTwoPickedTiles),
    sleep(10),
    determine_starter_player(
        PlayerOnePickedTiles, PlayerTwoPickedTiles, PlayerStarter, MaxTile
    ),
    play_automatic_starter_player(PlayerStarter, MaxTile),
    update_player_decision_matrix(player_one, MaxTile),
    update_player_decision_matrix(player_two, MaxTile),
    sleep(3),
    print_message('Organizing game table...'),
    sleep(3),
    start_game_loop.

/**
 * determine_starter_player(
 *  PlayerOnePickedTiles,
 *  PlayerTwoPickedTiles,
 *  PlayerStarter,
 *  MaxTile
 * ):-
 *
 *   Determine between the two players, who will start the game by
 *   the max tiles in both players hands.
 *
 *   INPUT:
 *     PlayerOnePickedTiles - Player one hand tiles.
 *     PlayerTwoPickedTiles - Player two hand tiles.
 *
 *   OUTPUT:
 *     PlayerStarter - The player which start the game between the two.
 *     MaxTile - The max tile found in the players hands.
 */
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
    print_visible_tile(MaxTile),
    print_message(''),
    sleep(1),
    print_message('The player which starts the game is: '),
    print_player_desc(PlayerStarter),
    print_message(''),
    set_current_turn(PlayerStarter).

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


% Play automatically for starter player.
% Basically put the tile on the game board and move the turn to the next
% player.
play_automatic_starter_player(PlayerStarter, MaxTile):-
    print_message(''),
    print_message_without_nl('Playing automatically for starter player: '),
    print_player_desc(PlayerStarter),
    print_message(''),
    sleep(5),
    print_player_desc(PlayerStarter),
    print_message_without_nl(' Put the tile '),
    print_board_tile(MaxTile),
    print_message_without_nl(' on the board'),
    print_message(''),
    assert(game_board([MaxTile | T1]-T1)),
    assert(last_tile_left(MaxTile)),
    assert(last_tile_right(MaxTile)),
    remove_tile_from_player_hand(PlayerStarter, MaxTile),
    set_next_turn.


% Main game loop - controls the game loop which manages the game.
start_game_loop:-
    cut_wrapper(print_game_screen),
    cut_wrapper(play_player_turn),
    cut_wrapper(set_next_turn),
    cut_wrapper(update_game_state),
    cut_wrapper(continue_game_loop).

% Play player turn for each player identity.
% If the player is a user, asking him for a possible move.
% If the player is a computer, calling for the computer move predicate.
play_player_turn:-
   curr_player_turn(CurrentPlayer),
   player_identity(CurrentPlayer, PlayerIdentity),
   print_message_without_nl('Current turn: '),
   print_player_desc(CurrentPlayer),
   print_message(''),
   (
       (
           PlayerIdentity == user_player,
           !,
           print_message('Calculating possible moves...'),
           sleep(3),
           ask_user_play(CurrentPlayer)
       )
       ;
       (
           !,
           perform_computer_play(CurrentPlayer)
       )
   ).

% Handles the game loop, due to the current game state.
continue_game_loop:-
    game_state(CurrentGameState),
    (
        (
            CurrentGameState == in_game,
            start_game_loop
        )
        ;
        (
            CurrentGameState == end_game,
            print_game_result
        )
    ).

% Updating current game state due to the last play performed.
update_game_state:-
    player_hand_count(player_one, PlayerOneCount),
    player_hand_count(player_two, PlayerTwoCount),
    (
        (
            PlayerOneCount == 0,
            !,
            set_game_state(end_game),
            set_game_result(player_one_win)
        )
        ;
        (
            PlayerTwoCount == 0,
            !,
            set_game_state(end_game),
            set_game_result(player_two_win)
        )
        ;
        (
            check_possible_tie(IsTie),
            !,
            IsTie == yes,
            (
                player_hand_weight(player_one, PlayerOneWeight),
                player_hand_weight(player_two, PlayerTwoWeight),
                (
                    (
                        PlayerOneWeight > PlayerTwoWeight,
                        GameResult = player_two_win
                    )
                    ;
                    (
                        PlayerTwoWeight > PlayerOneWeight,
                        GameResult = player_one_win
                    )
                    ;
                    (
                        GameResult = tie
                    )
                )
            ),
            set_game_state(end_game),
            set_game_result(GameResult)
        )
        ;
        (
            true
        )
    ).


% Checking possible tie option between the players and return if it is
% really a tie.
check_possible_tie(IsTie):-
    player_hand(player_one, PlayerOneHand),
    player_hand(player_two, PlayerTwoHand),
    get_appendable_tiles(PlayerOneHand, PlayerOneMoves),
    get_appendable_tiles(PlayerTwoHand, PlayerTwoMoves),
    game_boneyard_count(BoneyardCount),
    (
        (
            PlayerOneMoves == [],
            PlayerTwoMoves == [],
            BoneyardCount == 0,
            IsTie = yes
        )
        ;
        (
            IsTie = no
        )
    ).

% Asking the user for playing selection or automatic draw from boneyard.
ask_user_play(Player):-
    player_hand(Player, PlayerHand),
    get_appendable_tiles(PlayerHand, PossibleMoves),
    (
        (
            PossibleMoves == [],
            !,
            show_automatic_draw_boneyard(Player)


        )
        ;
        (
            !,
            print_message('Please select one of the following options to play: '),
            show_possible_moves_selection(PossibleMoves, 1-Range),
            get_user_selection(1-Range, Selection),
            nth1(Selection, PossibleMoves, Move),
            perform_user_play(Player, Move)
        )
    ).

/**
 * show_automatic_draw_boneyard(Player):-
 *   Shows automtatic draw from boneyard scenario for a given
 *   player.
 *
 *   INPUT:
 *     Player - The player to perform on automatic
 *              drawing from boneyard.
 */
show_automatic_draw_boneyard(Player):-
    print_message_without_nl('Sorry, no possible moves found for '),
    print_player_desc(Player),
    print_message(''),
    print_message('Automatically drawing bone from boneyard...'),
    sleep(5),
    pick_tiles_from_boneyard(1, [Tile]),
    add_tile_to_player_hand(Player, Tile).


/**
 * perform_user_play(Player, PossibleMove):-
 *   Perform a possible move by a given player.
 *
 *   INPUT:
 *     Player - The player which plays the given move.
 *     PossibleMove - The move which the player will play.
 */
perform_user_play(Player, possible_move(bone(LeftValue, RightValue), Side, Reversed)):-
    (
        (
            Reversed == yes,
            !,
            ParsedTile = bone(RightValue, LeftValue)
        )
        ;
        (
            !,
            ParsedTile = bone(LeftValue, RightValue)
        )
    ),
    print_selection_performed(Player, ParsedTile, Side),
    game_board(Board),
    append_tile_on_board(Board, ParsedTile, Side, _),
    remove_tile_from_player_hand(Player, bone(LeftValue, RightValue)),
    update_player_decision_matrix(player_one, ParsedTile),
    update_player_decision_matrix(player_two, ParsedTile).

/**
 * perform_computer_play(Player):-
 *   Perform a computer play move.
 *
 *   INPUT:
 *     Player - The actual player which plays as computer.
 */
perform_computer_play(Player):-
    player_hand(Player, PlayerHand),
    get_appendable_tiles(PlayerHand, PossibleMoves),
    print_message_without_nl('Player '),
    print_player_desc(Player),
    print_message(' is playing...'),
    sleep(3),
    (
        (
            PossibleMoves == [],
            !,
            show_automatic_draw_boneyard(Player)
        )
        ;
        (
            !,
            perform_specific_computer_move(Player, PossibleMoves)
        )
    ).

/**
 * perform_specific_computer_move(Player, PossibleMoves):-
 *   Perform a specific computer move, by a given player and
 *   possible moves.
 *
 *   INPUT:
 *     Player - the player which is the computer player.
 *     PossibleMoves - List possible moves for the given player.
 */
perform_specific_computer_move(Player, PossibleMoves):-
    player_identity(Player, PlayerIdentity),
    (
        (
            PlayerIdentity == computer_random,
            !,
            computer_random_move(Player, PossibleMoves)
        )
        ;
        (
            PlayerIdentity == computer_greedy,
            !,
            computer_greedy_move(Player, PossibleMoves)
        )
        ;
        (
            PlayerIdentity == computer_statistical,
            !,
            computer_statistical_move(Player, PossibleMoves)
        )
    ).

/**
 * computer_statistical_move(Player, PossibleMoves):-
 *   Playing a computer statistical move.
 *
 *   The computer statistical bot, plays by the following strategy:
 *
 *   First, he sorts all his possible moves by the weight
 *   of each bone tile.
 *   He does that because in situation of tie, the player with
 *   the lowest weight of tiles is winning.
 *
 *   Second, he sorts his probability row in his
 *   decision matrix for getting indication
 *   of which of the bone tiles has the lowest probability to be
 *   drawn by the other player.
 *
 *   He does that because the decision matrix holds in the last row
 *   a number which indicates the number of time a number in bone
 *   tile is showed in the game. (Each row indicates a number in a tile)
 *
 *   Then, he choose the possible move which has the
 *   higher possible probability to now be drawn from
 *   boneyard, and in the same time the most weighted
 *   bone tile (If possible).
 *
 *   INPUT:
 *     Player - The player which plays the computer statistical move.
 *     PossibleMoves - List possible moves of the player.
 *
 */
computer_statistical_move(Player, PossibleMoves):-
    matrix_dimensions(RowSize, _),
    ActualRowIndex is RowSize - 1,
    player_decision_matrix(Player, DecisionMatrix),
    get_row_from_decision_matrix(ActualRowIndex, DecisionMatrix, ProbabilityRow),
    get_best_move(ProbabilityRow, PossibleMoves, BestMove),
    perform_user_play(Player, BestMove).


/**
 * computer_random_move(Player, PossibleMoves):-
 *   Playing a computer random move.
 *
 *   The computer random bot, playes by the following strategy:
 *
 *   He choose a random selection from his possible moves, and
 *   plays it.
 *
 *   INPUT:
 *     Player - The player which plays the computer random move.
 *     PossibleMoves - List possible moves of the player.
 */
computer_random_move(Player, PossibleMoves):-
    list_length(PossibleMoves, Range),
    random_between(1, Range, RandomSelection),
    nth1(RandomSelection, PossibleMoves, Move),
    %perform_user_play(Player, PossibleMoves, RandomSelection).
    perform_user_play(Player, Move).

/**
 * computer_greedy_move(Player, PossibleMoves):-
 *   Playing a computer greedy move.
 *
 *   The computer greedy bot, plays by the following strategy:
 *
 *   He choose the possible move with the maximum weight and
 *   plays with it.
 *
 *   INPUT:
 *     Player - The player which plays the computer greedy move.
 *     PossibleMoves - List possible moves of the player.
 */
computer_greedy_move(Player, PossibleMoves):-
    max_in_list(weight_estimator, PossibleMoves, MaxWeightMove),
    perform_user_play(Player, MaxWeightMove).


/**
 * get_best_move(ProbabilityRow, PossibleMoves, BestMove):-
 *   Gets the best move a computer statistical bot can perform.
 *
 *   Takes the probability row of the player,
 *   Sorts it and the possible moves of the player
 *   (In descending order),
 *   Chooses the best move by backtracking on the both
 *   sorted lists.
 *
 *   INPUT:
 *     ProbabilityRow - The probability row of the player.
 *     PossibleMoves - List of possible moves of the player.
 *
 *   OUTPUT:
 *     BestMove - The best move the player can perform.
 */
get_best_move(ProbabilityRow, PossibleMoves, BestMove):-
    probability_parser(ProbabilityRow, ParsedProbabilityRow),
    merge_sort(weight_estimator, PossibleMoves, SortedPossibleMoves),
    merge_sort(probability_estimator, ParsedProbabilityRow, SortedProbabilityRow),
    cut_wrapper(find_best_probability_move(SortedProbabilityRow, SortedPossibleMoves, BestMove)).


/**
 * find_best_probability_move(ProbRow, PosMoves, Move):-
 *   Find the best probability move by sorted probability row
 *   and sorted possible moves lists.
 *
 *   INPUT:
 *     ProbRow - The probability row of the player.
 *     PosMoves - List of possible moves of the player.
 *
 *   OUTPUT:
 *     Move - The best move the player can perform.
 */
find_best_probability_move(ProbRow, PosMoves, Move):-
    (
		member((_, Index), ProbRow),
            member(possible_move(bone(Left, Right), Side, Reversed), PosMoves),
            (
			(
			Left == Index,
                    (

			(
				Side == left,
                            Reversed == no
                        )
			;
                        (
                            Side == right,
                            Reversed == yes
                        )
                    )
                )
		;
		(
			Right == Index,
                    (
			(
                            Side == right,
                            Reversed == no
                        )
			;
			(
                            Side == left,
                            Reversed == yes
                        )
                    )
                )
            )
    ),
    Move = possible_move(bone(Left, Right), Side, Reversed).


/**
 * probability_parser(ProbabilityRow, ParsedProbabilityRow):-
 *   Parses a probability row to a list of pairs, which
 *   each pair is in the following format:
 *   (Value, Index):
 *     Value - The value in the probability row.
 *     Index - The row index in the decision matrix.
 *
 *   INPUT:
 *     ProbabilityRow - The probability row to parse.
 *
 *   OUTPUT:
 *     ParsedProbabilityRow - The parsed probability row.
 */
probability_parser(ProbabilityRow, ParsedProbabilityRow):-
    probability_parser(ProbabilityRow, 0, ParsedProbabilityRow).

/**
 * probability_parser(ProbabilityRow, CurrIndex, ParsedProbabilityRow):-
 *   Helper for the `probability_parser` predicate.
 *
 *   INPUT:
 *     ProbabilityRow - The probability row to parse.
 *     CurrIndex - The current index in the probability row.
 *
 *   OUTPUT:
 *     ParsedProbabilityRow - The parsed probability row.
 */

% When there's only one value, the parsed list will be the pair of the
% value and the current index.
probability_parser([Value], Index, [(Value, Index)]):- !.

% Set each probability value, with it corresponding index in the parsed
% probability row.
probability_parser(
    [ProbabilityValue | RestRow],
    Index,
    [(ProbabilityValue, Index) | RestParsedRow]
):-
    !,
    NextIndex is Index + 1,
    probability_parser(RestRow, NextIndex, RestParsedRow).


/**
 * weight_estimator(PossibleMove1, PossibleMove2, MaxPossibleMove):-
 *   Weight estimator for the merge sort to unify the
 *   'greather-than' condition to sorts the possible moves lists.
 *
 *   It uses the weight of the bone as the estimator.
 *   The possible move with the bone with the higher weight will be the
 *   greather possible move.
 *
 *   INPUT:
 *     PossibleMove1 - A possible move.
 *     PossibleMove2 - Another possible move
 *
 *   OUTPUT:
 *     MaxPossibleMove - The maximum possible move between the two.
 */
weight_estimator(
    possible_move(bone(FirstLeft, FirstRight), FirstSide, FirstReversed),
    possible_move(bone(SecondLeft, SecondRight), SecondSide, SecondReversed),
    MaxPossibleMove
):-
    FirstSum is FirstLeft + FirstRight,
    SecondSum is SecondLeft + SecondRight,
    (
	(
            FirstSum >= SecondSum,
            MaxPossibleMove = possible_move(bone(FirstLeft, FirstRight), FirstSide, FirstReversed)
	)
        ;
	(
            SecondSum > FirstSum,
            MaxPossibleMove = possible_move(bone(SecondLeft, SecondRight), SecondSide, SecondReversed)
	)
    ).

/**
 * probability_estimator(
 *  (Value1, Index1),
 *  (Value2, Index2),
 *  (MaxValue, MaxIndex)
 * ):-
 *   Probability estimator for the probability row elements to unify
 *   the 'greather-than' condition to sorts the probability
 *   row elements.
 *
 *   It uses the value of each row index as the estimator.
 *   The higher pair of of Value & Index will be the greather
 *   pair.
 *
 *   INPUT:
 *     (Value1, Index1) - A pair of value and index.
 *     (Value2, Index2) - Another pair of value and index.
 *
 *   OUTPUT:
 *     (MaxValue, MaxValue) - The max pair between the two.
 *
 */
probability_estimator(
    (FirstValue, FirstIndex),
    (SecondValue, SecondIndex),
    (MaxValue, MaxIndex)
):-
    (
	(
            FirstValue >= SecondValue,
            MaxValue = FirstValue,
            MaxIndex = FirstIndex
        )
	;
	(
            MaxValue = SecondValue,
            MaxIndex = SecondIndex
        )
    ).

% Removes given tile from a given player's hand.
remove_tile_from_player_hand(Player, Tile):-
    player_hand(Player, PlayerHand),
    remove_element_from_list(Tile, PlayerHand, NewPlayerHand),
    retractall(player_hand(Player, _)),
    assert(player_hand(Player, NewPlayerHand)),
    change_player_hand_count(Player, -1),
    recalculate_and_set_player_hand_weight(Player, Tile, remove).

% Adds given tile to a given player's hand.
add_tile_to_player_hand(Player, Tile):-
    player_hand(Player, PlayerHand),
    append([Tile], PlayerHand, NewPlayerHand),
    retractall(player_hand(Player, _)),
    assert(player_hand(Player, NewPlayerHand)),
    change_player_hand_count(Player, 1),
    recalculate_and_set_player_hand_weight(Player, Tile, add).

/**
 * get_appendable_tiles(Tiles, AppendableTiles):-
 *   For a given tiles list, return the possible
 *   appendable tiles to the game board by a possible_move
 *   predicate which structured in the following manner:
 *
 *   possible_move(Tile, Side, Reversed):
 *   Tile - The bone tile which is part of the possible move.
 *   Side - The side which the bone can be placed in.
 *   Reversed - Indicates if the bone need to be in reverse
 *              Order or not, in the possible move.
 *
 *   INPUT:
 *     Tiles - A list of bone tiles.
 *
 *   OUTPUT:
 *     AppendableTiles - List of appendable tiles structured as
 *                       possible_move predicate which indicates
 *                       for the tiles given, which tile can be
 *                       placed in the current game board, and how.
 */
get_appendable_tiles(Tiles, AppendableTiles):-
    (
	setof(
            possible_move(Tile, Side, Reversed),
            (
		member(Tile, Tiles),
		check_appendable_tile(Tile, Side, Reversed, Validity),
		Validity \== novalid
            ),
            AppendableTiles
        )
	; % No one of the tiles given can be placed in the board.
	AppendableTiles = []
    ).

/**
 * check_appendable_tile(Tile, Side, Reversed, Validity):-
 *   Checking if a bone can be placed in the game board, and how.
 *
 *   INPUT:
 *     Tile - The bone tile to check if it can be placed in
 *            the game board.
 *
 *   OUTPUT:
 *     Side - Which side the tile can be placed in the game board.
 *            Values are: left/right.
 *     Reversed - Indicates if the tile need to be placed in
 *                reverse order.
 *                Values are: yes/no.
 *     Validity - Indicates if the bone can be actually placed
 *                in the game board.
 *                Values are: valid/novalid.
 */
check_appendable_tile(Tile, Side, Reversed, Validity):-

    % Getting the last tile placed in the game board.
    get_last_tiles(bone(LeftTileLeftValue, _), bone(_, RightTileRightValue)),
    Tile = bone(CheckLeftValue, CheckRightValue),
    (
	(
            % If the left value and the right value of the tile
            % are equals, than the bone is not needed to be reversed.
            CheckLeftValue =:= CheckRightValue,
            !,
            Reversed = no,
            (
		(
                    % Current side is right, and it equals to the
                    % number in the tile.
                    Side = right,
                    RightTileRightValue =:= CheckRightValue,
                    Validity = valid
                );
		(
                    % Current side is left, and it equals to the
                    % number in the tile.
                    Side = left,
                    LeftTileLeftValue =:= CheckLeftValue,
                    Validity = valid
                );
		(
                    % The tile not matched any of the numbers
                    % in the board last tiles.
                    % The bone cannot be placed in the game board.
                    Side = uninitialized,
                    Validity = novalid
                )
            )
        );
	(
            (
                % Regular bone, current side is right
                Side = right,
                (
                    (
                        % Last tile right value is the same as
                        % the current tile left value.
                        % The tile can be placed in the board at
                        % right side regulary.
                        RightTileRightValue =:= CheckLeftValue,
                        Reversed = no,
                        Validity = valid
                    );
                    (
                        % Last tile right value is the same as
                        % the current tile right value.
                        % The tile can be placed in the board at
                        % right side but in reverse order.
			RightTileRightValue =:= CheckRightValue,
                        Reversed = yes,
                        Validity = valid
                    )
                )
            );
            (
                % Regular bone, current side is left
		Side = left,
		(
                    (
                        % Last tile left value is the same as
                        % the current tile right value.
                        % The tile can be placed in the board at
                        % left side regulary.
			LeftTileLeftValue =:= CheckRightValue,
                        Reversed = no,
                        Validity = valid
                    );
                    (
                        % Last tile left value is the same as
                        % the current tile left value.
                        % The tile can be placed in the board at
                        % left side but in reverse order.
			LeftTileLeftValue =:= CheckLeftValue,
                        Reversed = yes,
                        Validity = valid
                    )
                )
            );
            (
                % The tile cannot be placed at the board.
		Side = uninitialized,
                Reversed = uninitialized,
                Validity = novalid
            )
	)
    ).


/** In-game Configurations & Calculations Predicates **/

% Automaticly setting the current turn of player by changing the
% player turn to be the opposite of the last player turn.
set_next_turn:-
    curr_player_turn(CurrentPlayer),
    (
        (
            CurrentPlayer == player_one,
            set_current_turn(player_two)
        );
        set_current_turn(player_one)
    ).

% Sets the PlayerID as the current turn player
set_current_turn(PlayerID):-
    retractall(curr_player_turn(_)),
    assert(curr_player_turn(PlayerID)).

% Sets the Player type for a given Player (player_one / player_two)
set_player_identity(Player, PlayerType):-
    retractall(player_identity(Player, _)),
    assert(player_identity(Player, PlayerType)).

% Sets the player's decision matrix
set_player_decision_matrix(Player, Matrix):-
    retractall(player_decision_matrix(Player, _)),
    assert(player_decision_matrix(Player, Matrix)).

% Sets the game boneyard for a given boneyard.
set_game_boneyard(Boneyard):-
    retractall(game_boneyard(_)),
    assert(game_boneyard(Boneyard)).

% Sets the game boneyard tiles count for a given number
set_game_boneyard_count(BoneyardCount):-
    retractall(game_boneyard_count(_)),
    assert(game_boneyard_count(BoneyardCount)).

% Sets the current game state value.
set_game_state(GameState):-
    retractall(game_state(_)),
    assert(game_state(GameState)).

% Set the game result value.
set_game_result(GameResult):-
    retractall(game_result(_)),
    assert(game_result(GameResult)).

% Sets if the computer tiles need to be visible in the game.
set_computer_tiles_visible(IsVisible):-
    retractall(computer_tiles_visible(_)),
    assert(computer_tiles_visible(IsVisible)).

% Toggle computer tiles visible from yes to no and no to yes.
toggle_computer_tiles_visible:-
    computer_tiles_visible(CompTilesVisible),
    (
        (
            CompTilesVisible == yes,
            set_computer_tiles_visible(no)
        )
        ;
        (
            set_computer_tiles_visible(yes)
        )
    ).

% Set player hand count by given player and count
change_player_hand_count(Player, Addition):-
    player_hand_count(Player, PrevCount),
    NewCount is PrevCount + Addition,
    retractall(player_hand_count(Player, _)),
    assert(player_hand_count(Player, NewCount)).

/**
 * get_hand_size(Tiles, Sum):-
 *   Returns the sum of a given tiles list.
 *   Sum is calculated by calculating the sum
 *   of each bone tile numbers in each side.
 *
 *   INPUT:
 *     Tiles - List of tiles.
 *
 *   OUTPUT:
 *     Sum - The sum of the given tiles.
 */

% Empty list have zero sum.
get_hand_size([], 0).

% Continue iterating the tiles list, while after returning back
% calculate the sum of all values reached.
get_hand_size([bone(LeftValue, RightValue) | RestTiles], Sum):-
    get_hand_size(RestTiles, RestSum),
    Sum is LeftValue + RightValue + RestSum.

% Calculate and set player hand weight by given player and tiles
% for the first time.
calculate_and_set_player_hand_weight(Player, PlayerTiles):-
    get_hand_size(PlayerTiles, Weight),
    retractall(player_hand_weight(Player, _)),
    assert(player_hand_weight(Player, Weight)).

% Recalculate and set player hand weight by given player and tile and
% operation (remove/add)
recalculate_and_set_player_hand_weight(
    Player,
    bone(LeftValue, RightValue),
    Operation
):-
    player_hand_weight(Player, Weight),
    TileSum is LeftValue + RightValue,
    (
        (
            Operation == add,
            NewWeight is Weight + TileSum
        )
        ;
        (
            NewWeight is Weight - TileSum
        )
    ),
    retractall(player_hand_weight(Player, _)),
    assert(player_hand_weight(Player, NewWeight)).

% Sets the default settings for the game to start
set_default_settings:-
    set_player_identity(player_one, user_player),
    set_player_identity(player_two, computer_random),
    set_computer_tiles_visible(no).

/**
 * generate_boneyard(RandomBoneyard):-
 *   Generates a random boneyard of bone tiles.
 *
 *   OUTPUT:
 *     RandomBoneyard - The generated random boneyard.
 */

% Generates random boneyard for the game
generate_boneyard(RandomBoneyard):-
    bagof(bone(X,Y), bone(X, Y), BoneList),
    random_permutation(BoneList, RandomBoneyard).

% Prepare player specific configuration for the game.
prepare_player(Player):-
    player_identity(Player, Identity),
    (
        (
            Identity == computer_statistical,
            generate_decision_matrix_for_player(Player)
        )
        ;
        (
            true
        )
    ).

% Generate a decision matrix for a given player.
generate_decision_matrix_for_player(Player):-
    generate_decision_matrix(Matrix),
    set_player_decision_matrix(Player, Matrix).

% Updating decision matrix of player with given bone tile.
update_player_decision_matrix(Player, bone(LeftSide, RightSide)):-
    player_identity(Player, Identity),
    (
        (
            Identity == computer_statistical,
            player_decision_matrix(Player, Matrix),
            update_element_in_decision_matrix(LeftSide, RightSide, Matrix, UpdatedMatrix),
            set_player_decision_matrix(Player, UpdatedMatrix)
        )
        ;
        (
            true
        )
    ).

% Decorating predicate for the main pick tiles from boneyard which
% updates the game boneyard.
pick_tiles_from_boneyard(NumTiles, ReturnedTiles):-
    game_boneyard(Boneyard),
    game_boneyard_count(BoneyardCount),
    NewBoneyardCount is BoneyardCount - NumTiles,
    pick_tiles_from_boneyard(NumTiles, Boneyard, NewBoneyard, ReturnedTiles),
    set_game_boneyard(NewBoneyard),
    set_game_boneyard_count(NewBoneyardCount).

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

/**
 * append_tile_on_board(
 *  GameBoard,
 *  Tile,
 *  Side,
 *  UpdatedGameBoard
 * ):-
 *   Append a tile to a given game board and side.
 *   Returns the updated game board.
 *   NOTE: Game board must be in difference-list format.
 *
 *   INPUT:
 *     GameBoard - The game board to append tile on.
 *     Tile - The tile to append to the game board.
 *     Side - The side which the tile will be append to.
 *
 *   OUTPUT:
 *     UpdatedGameBoard - The new game board as the result of
 *                        appending the given tile.
 */

% Appending the tile on the left side of the board.
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

% Appending the tile on the right side of the board.
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

/**
 * get_last_tiles(LeftTile, RightTile):-
 *   Get the last tile in the current game board, by
 *   extracting the last tile from the last_tile predicates.
 *
 *   OUTPUT:
 *     LeftTile - The tile which found on the left end of the board.
 *     RightTile - The tile which found on the right end of the board.
 */
get_last_tiles(LeftTile, RightTile):-
    last_tile_left(LeftTile),
    last_tile_right(RightTile).



/** Cleanup Predicates **/

% Cleanup whole game dynamic predicates from memory.
cleanup:-
    cleanup_game_states,
    retractall(computer_tiles_visible(_)),
    retractall(player_identity(_, _)).

% Cleanup game state dynamic predicates from memory.
cleanup_game_states:-
    retractall(game_state(_)),
    retractall(game_board(_)),
    retractall(game_boneyard(_)),
    retractall(game_boneyard_count(_)),
    retractall(game_result(_)),
    retractall(player_hand_count(_, _)),
    retractall(player_hand_weight(_, _)),
    retractall(player_hand(_, _)),
    retractall(curr_player_turn(_)),
    retractall(last_tile_right(_)),
    retractall(last_tile_left(_)),
    retractall(player_decision_matrix(_, _)).


/** Game Helpers **/

/**
 * get_user_selection(StartRange-EndRange, Selection):-
 *   Gets user selection until it is in the selection range.
 *
 *   INPUT:
 *     StartRange-EndRange - The range of possible selection the user
 *                           can select.
 *
 *   OUTPUT:
 *     Selection - The selection the user made.
 */
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


/**
 * validate_user_selection(Selection, From-To, Validity):-
 *   Validates if a given user selection is in range of From-To.
 *
 *   INPUT:
 *     Selection - the selection made by the user.
 *     From-To - A range of possiblities to select by the user.
 *               From indicates the start of the range,
 *               To indicates the end of the range.
 *               User input should be between them.
 *     Validity - Value indicating if the user entered valid selection.
 *                Values are:
 *                - valid: User entered valid selection.
 *                - novalid: User entered invalid selection.
 */
validate_user_selection(Selection, From-To, valid):-
    integer(Selection),
    Selection >= From,
    Selection =< To.

% Any other selection is invalid because it does not unify with the
% previous predicate.
validate_user_selection(_, _, novalid).


% Gets a user input and store it in the UserInput variable.
get_user_input(UserInput):-
	get(Char),
	keep_get_user_input(RestChars),
	append([Char], RestChars, CharacterList),
	name(UserInput, CharacterList).

% Helper predicate for keep getting user input after first input.
% Used for scenarios where the player can choose more than 9 possiblity
% chocies (2 or more digits count).
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

% Continue to read until the end of input
keep_get_useless_user_input:-
    get0(UselessChar),

    % Continue getting useless characters until reaching end of input
    % at new line ('\n' = 10)
    (
        (UselessChar = 10, !)
        ;
        (keep_get_useless_user_input)
    ).



/** UI Predicates **/

% Prints the instructions of the game.
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

% Prints the main options for the game in the main menu.
print_main_options:-
    cut_wrapper(print_message('Please, select one of the following options to proceed:')),
    cut_wrapper(print_message('1. New Game')),
    cut_wrapper(print_message('2. Instructions')),
    cut_wrapper(print_message('3. Settings')),
    cut_wrapper(print_message('4. Exit')),
    cut_wrapper(print_message('')).

% Prints the player types options for the player to select.
print_player_options:-
    print_message(''),
    print_message_without_nl('1. '),
    displayable_player_type(user_player, DisplayUser),
    print_message(DisplayUser),
    print_message_without_nl('2. '),
    displayable_player_type(computer_random, DisplayCompRand),
    print_message(DisplayCompRand),
    print_message_without_nl('3. '),
    displayable_player_type(computer_greedy, DisplayCompGreedy),
    print_message(DisplayCompGreedy),
    print_message_without_nl('4. '),
    displayable_player_type(computer_statistical, DisplayCompStats),
    print_message(DisplayCompStats),
    print_message('5. Exit Selection'),
    print_message('').

% Prints the settings option in the main section of settings screen.
print_settings_main_choices:-
    print_message(''),
    print_message('Please, select one of the following options to proceed:'),
    print_message('1. Change Player One Type'),
    print_message('2. Change Player Two Type'),
    print_message('3. Toggle computer tiles visibile'),
    print_message('4. Exit'),
    print_message('').

% Prints the settings screen and the current settings applied.
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
    print_message_without_nl('Computer tiles visible: '),
    computer_tiles_visible(CompTilesVisible),
    print_message(CompTilesVisible),
    print_message('############################').


% Print exit game message.
print_exit_game:-
    print_message('Bye, see you next time!').


% Prints the opening game message when the player starts the game.
print_open_game_message:-
    cut_wrapper(print_message('Welcome to Prolog-Domino Game!')),
    cut_wrapper(print_message('A Domino game implemented in Prolog, which is fun and challenging.')).

% Prints the game screen of the current game stage.
print_game_screen:-
    print_game_border,
    print_player_two_side,
    print_game_border,
    print_board_tiles,
    print_game_border,
    print_player_one_side,
    print_game_border,
    print_game_boneyard.

% Prints game border for the game screen.
print_game_border:-
    print_message('********************************************').

% Prints player two side in the game screen.
print_player_two_side:-
    print_player_desc(player_two),
    print_message(''),
    player_hand(player_two, PlayerTiles),
    print_player_full_hand(player_two, PlayerTiles),
    print_message('').

% Prints player one side in the game screen.
print_player_one_side:-
    player_hand(player_one, PlayerTiles),
    print_player_full_hand(player_one, PlayerTiles),
    print_message(''),
    print_player_desc(player_one),
    print_message('').

% Prints game boneyard in the game screen.
print_game_boneyard:-
    game_boneyard_count(BoneyardCount),
    print_message(''),
    print_message_without_nl('[ '),
    print_message_without_nl(BoneyardCount),
    print_message_without_nl(' '),
    print_hide_tile,
    print_message(' Boneyard Tiles ]').

% Prints player short description.
print_player_desc(Player):-
    displayable_player(Player, DisplayPlayer),
    player_identity(Player, PlayerIdentity),
    displayable_player_type(PlayerIdentity, DisplayPlayerType),
    print_message_without_nl(DisplayPlayer),
    print_message_without_nl(' ('),
    print_message_without_nl(DisplayPlayerType),
    print_message_without_nl(') ').

% Printing the game results after the game ended.
print_game_result:-
    game_result(GameResult),
    (
        (
            GameResult == player_one_win,
            print_message(''),
            print_message('Player 1 won the game!'),
            print_players_weights

        )
        ;
        (
            GameResult == player_two_win,
            print_message(''),
            print_message('Player 2 won the game!'),
            print_players_weights
        )
        ;
        (
            print_message(''),
            print_message('The game ended up with a tie!')
        )
    ).

% Prints the players hands weight.
print_players_weights:-
    player_hand_weight(player_one, PlayerOneWeight),
    player_hand_weight(player_two, PlayerTwoWeight),
    print_message(''),
    print_message_without_nl('Player 1 Weight: '),
    print_message(PlayerOneWeight),
    print_message_without_nl('Player 2 Weight: '),
    print_message(PlayerTwoWeight),
    print_message('').


/**
 * show_possible_moves_selection(PossibleMoves, Length):-
 *   Show possible moves selection for a player.
 *   Basically shows each move with corresponding number selection
 *   for the player to select one of them as his next move.
 *
 *   INPUT:-
 *     PossibleMoves - List of possible moves for the player to play.
 *
 *   OUTPUT:
 *     Length - The length of the possible moves list.
 *
 */
show_possible_moves_selection(PossibleMoves, 1-Length):-
    list_length(PossibleMoves, Length),
    print_possible_moves(0, PossibleMoves).


/**
 * print_possible_moves(
 *   Index,
 *   [
 *       possible_move(
 *           bone(LeftValue, RightValue),
 *           Side,
 *           Reversed
 *       )
 *       |
 *       RestMoves
 *   ]
 * ):-
 *   Prints each possible moves in details
 *   with it's corresponding index.
 *
 *   INPUT:
 *     Index - Current index in the possible moves list.
 *     PossibleMoves - Possible moves list to print.
 */
print_possible_moves(_, []).
print_possible_moves(Index, [possible_move(bone(LeftValue, RightValue), Side, Reversed) | RestMoves]):-
    NextIndex is Index + 1,
    print_message_without_nl(NextIndex),
    print_message_without_nl('. '),
    (
        (
            Reversed == yes,
            !,
            ParsedTile = bone(RightValue, LeftValue)
        )
        ;
        (
            !,
            ParsedTile = bone(LeftValue, RightValue)
        )
    ),
    displayable_bone(ParsedTile, DisplayTile),
    print_message_without_nl(DisplayTile),
    print_message_without_nl(' (side: '),
    print_message_without_nl(Side),
    print_message(')'),
    print_possible_moves(NextIndex, RestMoves),
    !.


/**
 * print_selection_performed(Player, ParsedTile, Side):-
 *   Prints the current play move which done by a given player.
 *
 *   INPUT:
 *     Player - The player which performed the move.
 *     ParsedTile - The bone tile which played by the player.
 *     Side - The side of which the player put the bone tile.
 */
print_selection_performed(Player, ParsedTile, Side):-
    print_message(''),
    print_player_desc(Player),
    displayable_bone(ParsedTile, DisplayParsedTile),
    print_message_without_nl(' put '),
    print_message_without_nl(DisplayParsedTile),
    print_message_without_nl(' on the '),
    print_message_without_nl(Side),
    print_message(' of the board'),
    sleep(3).

% Prints a hide tile for each tile in the given tiles list.
print_hide_tiles(Tiles):-
    forall(
        member(_, Tiles),
        print_hide_tile
    ).

% Prints each tile of the given tiles list.
print_visible_tiles(Tiles):-
    forall(
        member(Tile, Tiles),
        print_visible_tile(Tile)
    ).

% Print each tile of the game board tiles.
print_board_tiles:-
    game_board(BoardTiles),
    forall(
        member_diff_list(Tile, BoardTiles),
        print_board_tile(Tile)
    ),
    print_message('').

% Prints a player full hand tiles one by one.
print_player_full_hand(Player, Tiles):-
    player_identity(Player, Identity),
    (
        (
            Identity == user_player,
            !,
            print_visible_tiles(Tiles)
        )
        ;
        (
            !,
            (
                (
                    computer_tiles_visible(CompTilesVisible),
                    CompTilesVisible == yes,
                    print_visible_tiles(Tiles)
                )
                ;
                print_hide_tiles(Tiles)
            )
        )
    ).

% Prints a single visible tile to the screen, which represent a tile
% in the hand of a player.
print_visible_tile(Tile):-
    displayable_hand_bone(Tile, UnicodeValue),
    print_unicode_value(UnicodeValue).

% Prints a single board tile, which visible in the board section of
% the game screen.
print_board_tile(Tile):-
    displayable_bone(Tile, UnicodeValue),
    print_unicode_value(UnicodeValue).

% Prints a non visible tile (hide tile), which represent a tile
% in the hand of a player.
print_hide_tile:-
    displayable_hand_bone(hide_bone, UnicodeValue),
    print_unicode_value(UnicodeValue).

% Prints a unicode value to the screen in correct format.
% (Used for displaying the bone tiles correctly)
print_unicode_value(UnicodeValue):-
    atom_string(X, UnicodeValue),
    write(X).

% Prints a message to the screen with new line after it.
print_message(Message):-
    write(Message),
    nl.

% Prints a message to the screen without new line after it.
print_message_without_nl(Message):-
    write(Message).
