/*
 * Author: Shaked Manes
 * Purpose: Game Project for MAMAN 17
 * Date: 31/10/2020
 * Module: game.utils.pl
 * Description: Utilities module which used in the game project.
 *
 * This program was developed using SWI-Prolog, so it depends
 * on the SWI-Prolog Prolog implementation standards.
 *
 * Also note that this programs requires SWI-Prolog font size of 36
 * to run properly.
 */

:- module(
       'game.utils',
       [
           cut_wrapper/1,
           remove_element_from_list/3,
           matrix_dimensions/2,
           generate_decision_matrix/1,
           get_element_from_decision_matrix/4,
           get_row_from_decision_matrix/3,
           update_element_in_decision_matrix/4,
           operation_on_element_in_list/5,
           set_element_in_list/6,
           member_diff_list/2,
           list_length/2,
           max_in_list/3,
           merge_sort/3
       ]
   ).

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


/** Decision Matrix Predicates **/

% Decision matrix dimensions.
matrix_dimensions(8, 7).

/**
 * generate_decision_matrix(Matrix):-
 *   Generates a brand new empty decision matrix for the game.
 *
 *   OUTPUT:
 *     Matrix - The new empty decision matrix that was generated.
 */
% This is the most efficient way to create matrix.
generate_decision_matrix(Matrix):-
    Matrix = [
	[0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0]
    ].

/**
 * get_element_from_decision_matrix(RowNum, ColNum, Matrix, Value):-
 *   Retrieves a value by row number and column number in the given
 *   matrix.
 *
 *   INPUT:
 *     RowNum - The row number of the value to retrieve.
 *     ColNum - The column number of the value to retrieve.
 *     Matrix - The matrix which the value will be retrieved from.
 *
 *   OUTPUT:
 *     Value - The value retreived from the matrix.
 */

% Get element at specific location at the matrix.
% First get the row, and access the ColNum index in the selected row.
get_element_from_decision_matrix(RowNum, ColNum, Matrix, Value):-
    get_row_from_decision_matrix(RowNum, Matrix, SelectedRow),
    nth0(ColNum, SelectedRow, Value).

/**
 * get_row_from_decision_matrix(RowNum, Matrix, RequestedRow):-
 *   Retrieves a row from the matrix.
 *
 *   INPUT:
 *     RowNum - The row number to retrieve from the matrix.
 *     Matrix - The matrix which the row will be retrieved from.
 *
 *   OUPUT:
 *     RequestedRow - The actual row which retrieved from the matrix.
 */

% Get specific row from matrix.
% Access the RowNum index in the matrix.
get_row_from_decision_matrix(RowNum, Matrix, RequestedRow):-
    nth0(RowNum, Matrix, RequestedRow).


/**
 * update_element_in_decision_matrix(
 *      RowNum,
 *      ColNum,
 *      Matrix,
 *      NewMatrix
 * ):-
 *   Updates element in the decision matrix.
 *   Basically it updates the appearence of bone tile in the matrix
 *   by setting the corresponding row number and column number of the
 *   bone tile to 1 in the matrix (also setting the opposite
 *   combination - treat the row number as column and column as row,
 *   as the bone tile can be also considered in reverse order).
 *
 *   After that, also updating the last row in the matrix
 *   (which is also known as 'Probability Row').
 *   The last row contains sum of the column values to indicates
 *   the probability of each number in the tiles existing in the current
 *   situation.
 *
 *   INPUT:
 *     RowNum - Row number to update in the matrix.
 *     ColNum - Column number to update in the matrix.
 *     Matrix - The matrix to perform the update on.
 *
 *   OUTPUT:
 *     NewMatrix - The updated matrix after the update operation.
 */

% Update element in decision matrix.
update_element_in_decision_matrix(RowNum, ColNum, Matrix, NewMatrix):-
    % Retrieve the matrix row size and calcualte the actual index.
    matrix_dimensions(RowSize, _),
    ActualLastRowIndex is RowSize - 1,
    (
	(
            % Means this is a double bone tile
            RowNum == ColNum,
            !,

            % Get the selected row, and the summary row from the matrix
            get_row_from_decision_matrix(RowNum, Matrix, SelectedRow),
            get_row_from_decision_matrix(ActualLastRowIndex, Matrix, SummaryRow),

            % Update the value in the selected double tile (increase one)
            operation_on_element_in_list(ColNum, 1, add, SelectedRow, UpdatedRow),

            % Replace the updated row in the matrix
            operation_on_element_in_list(RowNum, UpdatedRow, replace, Matrix, UpdatedMatrix),

            % Update the summary row
            operation_on_element_in_list(RowNum, 1, add, SummaryRow, UpdatedSummaryRow),

            % Replace the summary row in the matrix
            operation_on_element_in_list(ActualLastRowIndex, UpdatedSummaryRow, replace, UpdatedMatrix, NewMatrix)
        )
	;
	(
            % Get the corresponding bone tiles rows (Reversed and regular bone).
            !,
            get_row_from_decision_matrix(RowNum, Matrix, SelectedRow),
            get_row_from_decision_matrix(ColNum, Matrix, SelectedCol),

            % Get the summary row.
            get_row_from_decision_matrix(ActualLastRowIndex, Matrix, SummaryRow),

            % Update selected row and col.
            operation_on_element_in_list(ColNum, 1, add, SelectedRow, UpdatedRow),
            operation_on_element_in_list(RowNum, 1, add, SelectedCol, UpdatedCol),

            % Update summary row with values appended in selected row and col.
            operation_on_element_in_list(RowNum, 1, add, SummaryRow, UpdatedSummaryRow1),
            operation_on_element_in_list(ColNum, 1, add, UpdatedSummaryRow1, UpdatedSummaryRow2),

            % Update matrix with updated row, updated col, updated summary row.
            operation_on_element_in_list(RowNum, UpdatedRow, replace, Matrix, UpdatedMatrix1),
            operation_on_element_in_list(ColNum, UpdatedCol, replace, UpdatedMatrix1, UpdatedMatrix2),
            operation_on_element_in_list(ActualLastRowIndex, UpdatedSummaryRow2, replace, UpdatedMatrix2, NewMatrix)
        )
    ).


/**
 * operation_on_element_in_list(Index, Value, Operation, List, NewList):-
 *   Perform operation on specific index in list with given value
 *   and return the result list.
 *
 *   INPUT:
 *     Index - The element's index to perform operation on.
 *     Value - The value to use in the operation.
 *     Operation - The operation to perfrom to the element.
 *                 One of the two:
 *                 - add: adding the value to the element.
 *                 - replace: replace the element with the value.
 *     List - The list to perform the operation on.
 *   OUTPUT:
 *     NewList - The result list after the operation.
 */
operation_on_element_in_list(Index, Value, Operation, List, NewList):-
    set_element_in_list(Index, 0, Value, Operation, List, NewList).


/**
 * set_element_in_list(
 *   Index,
 *   CurrIndex,
 *   Value,
 *   Operation,
 *   List,
 *   NewList
 * ):-
 *   Helper predicate for the `operation_on_element_in_list` predicate.
 *
 *   INPUT:
 *     Index - The element's index to perform operation on.
 *     CurrIndex - The current index reached.
 *     Value - The value to use in the operation.
 *     Operation - The operation to perfrom to the element.
 *                 One of the two:
 *                 - add: adding the value to the element.
 *                 - replace: replace the element with the value.
 *     List - The list to perform the operation on.
 *
 *   OUTPUT:
 *     NewList - The result list after the operation.
 *
 */

% Empty list, no-op performed.
set_element_in_list(_, _, _, _, [], []):- !.

% Reached other index than the requested index, continue iterating.
set_element_in_list(
    Index,
    OtherIndex,
    Value,
    Operation,
    [Element | List],
    [Element | NewList]
):-
    OtherIndex \== Index,
    !,
    NextIndex is OtherIndex + 1,
    set_element_in_list(Index, NextIndex, Value, Operation, List, NewList).

% Reached the requested index, perform the operation and continue copy
% the elements to the new list.
set_element_in_list(
    Index,
    Index,
    Value,
    Operation,
    [Element | List],
    [OtherElement | NewList]
):-
    (
	(
            % Replace operation
            Operation == replace,
            !,
            OtherElement = Value
        )
	;
	(
            % Add operation
            OtherElement is Element + Value,
            !
        )
    ),
    NextIndex is Index + 1,
    set_element_in_list(Index, NextIndex, Value, Operation, List, NewList).

/**
 * member_diff_list(Element, List-RestList):-
 *   The usual member predicate, but for lists in format of
 *   difference-lists.
 */

% The element found in the list.
member_diff_list(Element, List-RestList):-
    List \== RestList,
    List = [Element | _].

% The element did not found yet in the list, continue to rest of the
% list.
member_diff_list(Element, List-RestList):-
    List \== RestList,
    List = [_ | Tail],
    member_diff_list(Element, Tail-RestList).

/**
 * list_length(List, Length):-
 *   Returns the length of given list.
 *
 *   INPUT:
 *     List - A list.
 *
 *   OUTPUT:
 *     Length - The length of the given list.
 */

% Empty list, lenght of zero.
list_length([], 0).

% List contains element, count it and continue iterating.
list_length([_ | Rest], Length):-
    list_length(Rest, RestLength),
    Length is RestLength + 1.

/**
 * max_in_list(Estimator, List, Max):-
 *   Finds the maximum element in given List, by given estimator.
 *   The predicates checks the 'greather-than' condition by the given
 *   estimator to find the maximum element in the list.
 *
 *   INPUT:
 *     Estimator - The estimator to run when checking 'greather-than'
 *                 condition.
 *     List - The list which the predicate search for maximum element.
 *
 *   OUTPUT:
 *     Max - The maximum element which found in the list.
 */
% List with only one element - that element is the max
max_in_list(_, [Max],Max):- !.

% Get the maximum in the rest of the list, and the first element.
% Compare them using the estimator given, which will return in Max
% the greather value between them.
max_in_list(Estimator, [Element | RestElements], Max):-
    max_in_list(Estimator, RestElements, RestMax),
    call(Estimator, Element, RestMax, Max),
    !.


/**
 * merge_sort_util_merge(
 *   Estimator,
 *   MergedList,
 *   List1,
 *   List2
 * ):-
 *   Merge sort utility predicate for merging sorted lists.
 *
 *   INPUT:
 *     Estimator - A predicate which estimate the 'greather-than'
 *                 condition which defines the sort.
 *     List1 - A list to merge.
 *     List2 - A list to merge.
 *
 *   OUTPUT:
 *     MergedList - The merged list which contains the two lists given
 *                  sorted in one list.
 */

% One of the lists is empty, the merge is the actual filled list of
% them.
merge_sort_util_merge(_, List, List, []).
merge_sort_util_merge(_, List, [], List).

% First list has the element which is the greater one.
merge_sort_util_merge(
    Estimator,
    [ElemList1 | RestMerged],
    [ElemList1 | RestList1],
    [ElemList2 | RestList2]
):-
    call(Estimator, ElemList1, ElemList2, ElemList1),
    !,
    merge_sort_util_merge(
        Estimator,
        RestMerged,
        RestList1,
        [ElemList2 | RestList2]
    ),
    !.

% Second list has the element which is the greather one.
merge_sort_util_merge(
    Estimator,
    [ElemList2 | RestMerged],
    [ElemList1 | RestList1],
    [ElemList2 | RestList2]
):-
    call(Estimator, ElemList2, ElemList1, ElemList2),
    !,
    merge_sort_util_merge(
        Estimator,
        RestMerged,
        [ElemList1 | RestList1],
        RestList2
    ),
    !.

/**
 * merge_sort(Estimator, List, Sorted):-
 *   Merge sort predicate, which sorts a given list by given estimator
 *   predicate which sets the condition of 'greather-than' to define
 *   the sort order.
 *
 *   INPUT:
 *     Estimator - A predicate which estimate the 'greather-than'
 *                 condition which defines the sort.
 *     List - A list to sort.
 *
 *   OUTPUT:
 *     Sorted - The list given in sorted order.
 */

% Sort empty or one element lists, return the same list.
merge_sort(_, [], []).
merge_sort(_, [Element], [Element | []]).

% Using official merge sort algorithm to sort the list.
merge_sort(Estimator, List, Sorted):-
    !,
    length(List, N),
    !,
    FirstLength is N // 2,
    SecondLength is N - FirstLength,
    length(FirstUnsorted, FirstLength),
    !,
    length(SecondUnsorted, SecondLength),
    !,
    append(FirstUnsorted, SecondUnsorted, List),
    !,
    merge_sort(Estimator, FirstUnsorted, FirstSorted),
    !,
    merge_sort(Estimator, SecondUnsorted, SecondSorted),
    !,
    merge_sort_util_merge(Estimator, Sorted, FirstSorted, SecondSorted).
