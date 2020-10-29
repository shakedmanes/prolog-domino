/*
 * Author: Shaked Manes
 * Purpose: Game Project for MAMAN 17
 * Date: 31/10/2020
 * Module: game.config.pl
 * Description: Configurations module which used in the game project.
 *
 * This program was developed using SWI-Prolog, so it depends
 * on the SWI-Prolog Prolog implementation standards.
 *
 * Also note that this programs requires SWI-Prolog font size of 36
 * to run properly.
 */
:- module(
       'game.config',
       [
           bone/2,
           displayable_bone/2,
           displayable_hand_bone/2,
           displayable_player/2,
           displayable_player_type/2
       ]
   ).

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
displayable_player(player_one, 'Player 1').
displayable_player(player_two, 'Player 2').

/** Displaying players types **/
displayable_player_type(user_player, 'You').
displayable_player_type(computer_random, 'Computer Random Bot').
displayable_player_type(computer_greedy, 'Computer Greedy Bot').
displayable_player_type(computer_statistical, 'Computer Statistical Bot').
