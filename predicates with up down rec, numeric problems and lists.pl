%LB2

% Exercise 1

%max3(+X,+Y,+U,-Z) - puts in U max out of X, Y and Z
max3(X,Y,U,X):-X>Y,X>U,!.
max3(_,Y,U,Y):-Y>U,!.
max3(_,_,U,U).

%fact(+N,+X) where X is factorial of N
fact_up(0,1):-!.
fact_up(N,X):-N1 is N-1,fact_up(N1,X1),X is N*X1.

fact_down(N,X):-fact_down(0,1,N,X).
fact_down(N,Y,N,Y):-!.
fact_down(I,Y,N,X):-I1 is I+1, Y1 is Y*I1, fact_down(I1,Y1,N,X).

%sum_of_digits(+X,+S) - counts in S sum of digits of N
sum_of_digits_up(0, Y):-Y=0, !.
sum_of_digits_up(X, Y):-Z1 is X mod 10, Z2 is X div 10, sum_of_digits_up(Z2, Q), Y is Z1+Q.

% read_list(-List, +N) - read N elements and input them in List
read_list(List,N):-read_list(List,N,0,[]).
read_list(List,N,N,List):-!.
read_list(List,N,K,NewList):-read(X), append(NewList,[X],AppendListResult),K_next is K+1, read_list(List,N,K_next,AppendListResult).

% write_list(+List) - print elements of List
write_list([]):-!.
write_list([H|T]):-write(H),write(" "), write_list(T).

% sum_list_up(+List,?Sum) - checks if Sum is sum of all elements of List, or if -Sum => counts the sum and gives it to Sum
sum_list_up([],0):-!.
sum_list_up([H|T],Sum):-sum_list_up(T,SumT),Sum is H+SumT.

% sum_list_down(+List,?Sum) - checks if Sum is sum of elements of List, or if -Sum => counts sum and gives it to Sum
sum_list_down(List,Sum):-sum_list_down(List,0,Sum).
sum_list_down([],Sum,Sum):-!.
sum_list_down([H|T],CurSum,Sum):-NewSum is CurSum+H,sum_list_down(T,NewSum,Sum).

% delete_elements_with_sum(+List,+Elem,-ListForInput)- delete elements
% where sum_of_digits_up equals Elem
delete_elements_with_sum([],_,[]):-!.

delete_elements_with_sum([H|T],Elem,NewList) :-
    sum_of_digits_up(H, Sum),
    Sum =\= Elem,
    delete_elements_with_sum(T,Elem,NewList1),
    NewList = [H|NewList1].

delete_elements_with_sum([H|T], Elem, NewList) :-
    sum_of_digits_up(H, Sum),
    Sum =:= Elem,
    delete_elements_with_sum(T, Elem, NewList),!.

% Exercise 2 - Var 12

%min_digit(+Elem,-Min)- finds min from digits of Elem.
min_digit(Elem,Min):- Elem< 10,Min is Elem,!.

min_digit(Elem, Min) :-
    NextDigit is Elem mod 10,
    NextElem is Elem // 10,
    min_digit(NextElem, NextMin),
    (NextDigit < NextMin -> Min = NextDigit ; Min = NextMin).

% kolv_of_less_than_3(+Elem,-Kolv)- finds amount of digits that are less
% than 3
kolv_of_less_than_3(0,0).

kolv_of_less_than_3(Elem, Kolv) :-
    NextDigit is Elem mod 10,
    NextElem is Elem // 10,
    kolv_of_less_than_3(NextElem, NextKolv),
    (NextDigit < 3 -> Kolv is NextKolv+1;Kolv is NextKolv).


% Exercise 3 - Var 12 - 12, 13, 36

% Exercise 1.12 - найти макс и мин массива, а
% числа между ними записать в обратном пор€дке в новый
% массив

% min_element(+List,-Min) - finds min in List

min_element([Min], Min).
min_element([H|T], Min) :-
    min_element(T, Min1),
    Min is min(H, Min1).

% max_element(+List,-Max) - finds max in List
max_element([Max], Max).
max_element([H|T], Max) :-
    max_element(T, Max1),
    Max is max(H, Max1).

% reverse_between_min_max(?Min, ?Max, +InputList, -Result) - change places of elements between max and min
reverse_between_min_max(_, _, [], []).
reverse_between_min_max(Min, Max, [H|T], Result) :-
    H > Min, H < Max,
    reverse_between_min_max(Min, Max, T, R1),
    append(R1, [H], Result).
reverse_between_min_max(Min, Max, [H|T], Result) :-
    ( H =< Min ; H >= Max ),
    reverse_between_min_max(Min, Max, T, Result).

% reverse_array_between_min_max(+List, -Result) - min_element + max_element + reverse_between_min_max
reverse_array_between_min_max(InputList, Result) :-
    min_element(InputList, Min),
    max_element(InputList, Max),
    reverse_between_min_max(Min, Max, InputList, Result),!.


% Exercise 1.13 - разместить элементы, расположенные до мин, в конце массива

%move_elements_after_min(+List, ?Min, -Res) - puts elements before MIN after MIN
move_elements_after_min([], _, []).
move_elements_after_min([Min|T], Min, Result) :-
    move_elements_after_min(T, Min, Result).
move_elements_after_min([H|T], Min, Result) :-
    H \= Min,
    move_elements_after_min(T, Min, R1),
    append([H], R1, Result).

% move_elements_to_end_after_min(+List, -Res) - min_element + move_elements_after_min
move_elements_to_end_after_min(InputList, Result2) :-
    min_element(InputList, Min),
    move_elements_after_min(InputList, Min, Result),
    append([Min],Result,Result2),
    !.


% Exercise 1.36 ƒан целочисленный массив. Ќеобходимо найти максимальный нечетный элемент.

%odd_elem(+Elem) - checks if Elem is odd
odd_elem(Elem):-Elem mod 2 =:=1.

% max_odd_element(+List,-Max) - find max odd elem of List, if none -> NotFound
max_odd_element([H|T], Max) :- odd_elem(H),
    max_odd_element(T, H, Max),!; max_odd_element(T, "NotFound", Max),!.

max_odd_element([], Max, Max).
max_odd_element([H|T], CurrMax, Max) :-
    odd_elem(H),
    H > CurrMax,
    max_odd_element(T, H, Max).
max_odd_element([H|T], CurrMax, Max) :-
    (not(odd_elem(H)) ; H =< CurrMax),
    max_odd_element(T, CurrMax, Max).
