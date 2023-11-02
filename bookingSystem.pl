:- use_module(library(clpfd)).

%The below clauses are not together
:- discontiguous monthThirty/3.
:- discontiguous monthThirtyOne/3.
:- discontiguous monthTwentyEight/3.
:- discontiguous dayThirtyOne/3.
:- discontiguous dayThirty/3.
:- discontiguous dayTwentyEight/3.

%defines the days of months with 28 days, 30 days and 31 days
dayThirtyOne(X) --> [X], [st], {integer(X), X = 1}.
dayThirty(X) --> [X], [st], {integer(X), X = 1}.
dayTwentyEight(X) --> [X], [st], {integer(X), X = 1}.


dayThirtyOne(X) --> [X], [nd], {integer(X), X = 2}.
dayThirty(X) --> [X], [nd], {integer(X), X = 2}.
dayTwentyEight(X) --> [X], [nd], {integer(X), X = 2}.

dayThirtyOne(X) --> [X], [rd], {integer(X), X = 3}.
dayThirty(X) --> [X], [rd], {integer(X), X = 3}.
dayTwentyEight(X) --> [X], [rd], {integer(X), X = 3}.

dayThirty(X) --> [X], [th], {integer(X), X > 3, X < 31}.
dayThirtyOne(X) --> [X], [th], {integer(X), X > 3, X < 32}.

dayThirtyOne(X) --> [X], {integer(X), X > 0, X < 31}.
dayThirty(X) --> [X], {integer(X), X > 0, X < 30}. 
dayTwentyEight(X) --> [X], {integer(X), X > 0, X < 29}.

%Assuming this year February has 28 days
dayTwentyEight(X) --> [X], [th], {integer(X), X > 3, X < 29}.


%defines the months which will contain 28 days, 30 days and 31 days
monthThirtyOne(Y) --> [X], {X = january, Y = 1}.
monthTwentyEight(Y) --> [X], {X = february, Y = 2}.
monthThirtyOne(Y) --> [X], {X = march, Y = 3}.
monthThirty(Y) --> [X], {X = april, Y = 4}.
monthThirtyOne(Y) --> [X], {X = may, Y = 5}.
monthThirty(Y) --> [X], {X = june, Y = 6}.
monthThirtyOne(Y) --> [X], {X = july, Y = 7}.
monthThirtyOne(Y) --> [X], {X = august, Y = 8}.
monthThirty(Y) --> [X], {X = september, Y = 9}.
monthThirtyOne(Y) --> [X], {X = october, Y = 10}.
monthThirty(Y) --> [X], {X = november, Y = 11}.
monthThirtyOne(Y) --> [X], {X = december, Y = 12}.

monthThirty(X) --> [X], {integer(X), X = 4}.
monthThirty(X) --> [X], {integer(X), X = 6}.
monthThirty(X) --> [X], {integer(X), X = 9}.
monthThirty(X) --> [X], {integer(X), X = 11}.

monthThirtyOne(X) --> [X], {integer(X), X = 1}.
monthThirtyOne(X) --> [X], {integer(X), X = 3}.
monthThirtyOne(X) --> [X], {integer(X), X = 5}.
monthThirtyOne(X) --> [X], {integer(X), X = 7}.
monthThirtyOne(X) --> [X], {integer(X), X = 8}.
monthThirtyOne(X) --> [X], {integer(X), X = 10}.
monthThirtyOne(X) --> [X], {integer(X), X = 12}.

monthTwentyEight(X) --> [X], {integer(X), X = 2}.

%forTestingPurpose  month(Y, [april],[]).

%date(?X,?Y)
%defines the date with month and day
date(_,_) --> [].
date(X,Y) --> monthThirtyOne(X), dayThirtyOne(Y).
date(X,Y) --> monthThirty(X), dayThirty(Y).
date(X,Y) --> monthTwentyEight(X), dayTwentyEight(Y).

date(X,Y) --> dayThirtyOne(X), monthThirtyOne(Y).
date(X,Y) --> dayThirty(X), monthThirty(Y).
date(X,Y) --> dayTwentyEight(X), monthTwentyEight(Y).

date(X,Y) --> dayThirtyOne(X), [/], monthThirtyOne(Y).
date(X,Y) --> dayThirty(X), [/], monthThirty(Y).
date(X,Y) --> dayTwentyEight(X), [/], monthTwentyEight(Y).

date(X,Y) --> dayThirtyOne(X), [of], monthThirtyOne(Y).
date(X,Y) --> dayThirty(X), [of], monthThirty(Y).
date(X,Y) --> dayTwentyEight(X), [of], monthTwentyEight(Y).

%time(?Z,?Y)
%defines the time with hour and minute. We avoided seconds as in our project scenario it seems unnecessary
time(_,_) --> [].
time(X,Y) --> [X] ,[am], {integer(X), X > 0, X < 13, Y = 0}.
time(Z,Y) --> [X] ,[pm], {integer(X), X > 0, X < 13, Y = 0, Z is X + 12}.
time(Z,Y) --> [X] ,[oclock], {integer(X), X > 0, X < 13, Y = 0, Z is X + 12}.
time(X,Y) --> [X] , [:] , [Y] , {integer(X), X > 0, X < 25} , {integer(Y), Y > -1, Y < 61}.
time(X,Y) --> [X] ,[:], [Y] ,[am], {integer(X), X > 0, X < 13} , {integer(Y), Y > -1, Y < 61}.
time(Z,Y) --> [X] ,[:], [Y] ,[pm], {integer(X), X > 0, X < 13} , {integer(Y), Y > -1, Y < 61, Z is X + 12}.
time(X,Y) --> [X] , [Y] ,[am], {integer(X), X > 0, X < 13} , {integer(Y), Y > -1, Y < 61}.
time(X,Y) --> [X] , [Y] ,[pm], {integer(X), X > 0, X < 13} , {integer(Y), Y > -1, Y < 61}.

%numberOfPerson(?X)
%defines the number of person for each booking
numberOfPerson(_) --> [].
numberOfPerson(X) --> [X], {integer(X), X > 0}, randomWord.

article --> [].
article --> [the].
article --> [an].
article --> [a].

preposition --> [].
preposition --> [on], article.
preposition --> [in], article.
preposition --> [at], article.
preposition --> [for], article.
preposition --> [of], article.

stand --> [].
stand --> [table].
stand --> [a, table].

request --> [].
request --> [book].
request --> [please], [can], [we], [have].
request --> [can], [i], [book].
request --> [reserve], [us].
request --> [we], [would], [like].
request --> [reservation].

randomWord --> [].
randomWord --> [people].
randomWord --> [please].
randomWord --> [preferably].
randomWord --> [party].
randomWord --> [us].

person --> [us].

menu(_) --> [].
menu(X) --> [X], [menu], randomWord, {X = theatre}.
menu(X) --> [X],[menu], randomWord, {X = standard}.

%reserveSeat(?A,?D,?X,?Y,?B,?C)
%this predicate defines different sentences which can be made using dcg. It returns the values on each Constants.
reserveSeat(A,D,X,Y,B,C) -->  request, stand, preposition ,numberOfPerson(A),preposition, menu(D), time(X,Y),preposition, date(B,C).
reserveSeat(A,X,Y,B,C,D) -->  request, stand, preposition, time(A,X), preposition, numberOfPerson(Y), preposition, date(B,C), preposition, menu(D).
reserveSeat(A,X,Y,B,C,D) -->  request, numberOfPerson(A), preposition, randomWord, preposition, preposition, date(X,Y), preposition, time(B,C), menu(D).
reserveSeat(A,X,Y,D,B,C) -->  request, preposition, numberOfPerson(A), preposition, date(X,Y), randomWord, preposition, menu(D), preposition, time(B,C).
reserveSeat(A,X,Y,D,B,C) -->  request, stand, preposition, date(A,X), preposition, randomWord, preposition, numberOfPerson(Y), preposition, menu(D), time(B,C).
reserveSeat(A,X,Y,B,C,D) -->  request, stand, preposition, numberOfPerson(A), preposition, date(X,Y), time(B,C), menu(D).

%booking(1, [?NumberOfPerson,?TimeHour,?TimeMinute,?Day,?Month,?Menu]) 
%These are the 8 booking requests which are ordered from 1 - 7.
booking(1, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]) :- 
    reserveSeat(NumberOfPerson, Menu,TimeHour,TimeMinute,Day,Month, [table,for,3,at,20,:,00,on,18,march],[]).

booking(2, [NumberOfPerson,TimeHour,TimeMinute,Day,Month, Menu]) :- 
    reserveSeat(NumberOfPerson,Menu, Month, Day, TimeHour, TimeMinute ,[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).

booking(3, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]) :- 
    reserveSeat(NumberOfPerson,TimeHour,TimeMinute, Day, Month,Menu, [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,/,03],[]).

booking(4, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]) :- 
    reserveSeat(TimeHour,TimeMinute,NumberOfPerson,Day,Month, Menu,[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please],[]).

booking(5, [NumberOfPerson,TimeHour,TimeMinute,Day,Month, Menu]) :- 
    reserveSeat(Month, Day, NumberOfPerson, Menu, TimeHour,TimeMinute, [reserve,us,a,table,on,march,18,for,a,party,of,4,for,the,standard,menu],[]).

booking(6, [NumberOfPerson,TimeHour,TimeMinute,Day,Month, Menu]) :- 
    reserveSeat(NumberOfPerson,Day,Month,TimeHour,TimeMinute,Menu, [book,6,of,us,in,on,18,march,at,20,:,00],[]).

booking(7, [NumberOfPerson,TimeHour,TimeMinute,Day,Month, Menu]) :- 
    reserveSeat(NumberOfPerson,Month,Day,Menu,TimeHour,TimeMinute,[reservation,for,7,on,march,18,preferably,for,standard,menu,at,7,oclock],[]).


%For the orders, we will take it as whoever will order first will get priority. 1 will get priority than 2,3,4,.... Again 5 will get more priority than 6.


%scheduler(?N)
%This is the main predicate for the scheduler for the bookings
scheduler(N):- doSchedule(N).

%doSchedule(?N)
%This predicate will be called always for the first order
doSchedule(N):-
    N #= 1,
    booking(N, [_,TimeHour,_,_,_,_]),
    bookAccordingToTime(N),
    bookAccordingToPeople(N),
    timeCheckWithPreviousOrder(N, _, TimeHour).

%doSchedule(?N)
%For Other Orders this predicate will be called
doSchedule(N):- 
    N #> 1,
    bookAccordingToTime(N),
    bookAccordingToPeople(N),
    S is N - 1,
    timeCheckWithPreviousOrderWithTime(N, S).

%doSchedule(?N)
%For bookings without time, it will check the previous orders and set a time accordingly.
doSchedule(N):- 
    N #> 1,
    booking(N, [_,TimeHour,_,_,_,_]),
    TimeHour \== 17,
    TimeHour \== 18,
    TimeHour \== 19,
    TimeHour \== 20,
    TimeHour \== 21,
    TimeHour \== 22,
    TimeHour \== 23,
    bookAccordingToPeople(N),
    S is N - 1,
    timeCheckWithPreviousOrder(N, S, _).

%doSchedule(?N)   
%If any booking has more than 4 person than this predicate is called
doSchedule(N):- 
    cannotBook(N).

%timeCheckWithPreviousOrder(?N, ?S, ?NewTime)
timeCheckWithPreviousOrder(N, S, NewTime):-
    S = 0,
    format('Reservation can be booked for order number ~w at time ~w.', [N, NewTime]).

%timeCheckWithPreviousOrder(?N, ?S, ?NewTime)
%For bookings without time here it checks previous order times and then take a time between them. If the previous order has a time with standard menu, then we will choose their time added by 2
% for the current bookings time.
timeCheckWithPreviousOrder(N, S, NewTime):- 
    N #> 1,
    booking(S, [_,TimeforS,_,_,_,Menu]),
    Menu == standard,
    NewTime is TimeforS + 2,
    NewS is S - 1,
    timeCheckWithPreviousOrder(N, NewS, NewTime).

%timeCheckWithPreviousOrder(?N, ?S, ?NewTime)
%For bookings without time here it checks previous order times and then take a time between them. If the previous order has a time with theatre menu, then we will choose their time added by 1
% for the current bookings time.
timeCheckWithPreviousOrder(N, S, NewTime):- 
    N #> 1,
    booking(S, [_,TimeforS,_,_,_,Menu]),
    Menu == theatre,
    NewTime is TimeforS + 1,
    NewS is S - 1,
    timeCheckWithPreviousOrder(N, NewS, NewTime).

%timeCheckWithPreviousOrder(?N, ?S, ?NewTime)
%For bookings without time here it checks previous order times and then take a time between them. If the previous order has a time with no menu defined, then we will choose their time added by 2
% for the current bookings time.
timeCheckWithPreviousOrder(N, S, NewTime):- 
    N #> 1,
    booking(S, [_,TimeforS,_,_,_,Menu]),
    Menu \== theatre,
    Menu \== standard,
    NewTime is TimeforS + 2,
    NewS is S - 1,
    timeCheckWithPreviousOrder(N, NewS, NewTime).

%timeCheckWithPreviousOrderWithTime(?N, ?S)
%For any booking it checks if the time and table matches with the previous order.
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [NumberOfN,TimeforN,_,_,_,_]),
    booking(S, [NumberOfS,TimeforS,_,_,_,_]),
    TimeforN #\= TimeforS,
    NumberOfN #\= NumberOfS,
    T is S - 1,
    timeCheckWithPreviousOrderWithTime(N, T).

%timeCheckWithPreviousOrderWithTime(?N, ?S)
%checks if the time is equal but table is different. Number of people defines the table. For example: 2 person table is table 1, 3 person table is table 2, 4 person table is table 3
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [NumberOfN,TimeforN,_,_,_,_]),
    booking(S, [NumberOfS,TimeforS,_,_,_,_]),
    TimeforN #= TimeforS,
    NumberOfN #\= NumberOfS,
    format('Reservation can be booked for order number ~w at the desired time but on a different table as it is pre occupied.', [N]),
    !.

%checks if the time collides with previous order's time
%timeCheckWithPreviousOrderWithTime(?N, ?S)
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [_,TimeforN,_,_,_,_]),
    booking(S, [_,TimeforS,_,_,_,_]),
    TimeforN #\= TimeforS,
    T is S - 1,
    timeCheckWithPreviousOrderWithTime(N, T).

%timeCheckWithPreviousOrderWithTime(?N, ?S)
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [_,_,_,_,_,_]),
    booking(S, [_,_,_,_,_,_]),
    T is S - 1,
    timeCheckWithPreviousOrderWithTime(N, T).

%check if the time and table is equal with the previous orders. Also check the menu fromn the previous orders to get the value of the vacant time
%timeCheckWithPreviousOrderWithTime(?N, ?S)
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [NumberOfN,TimeforN,_,_,_,_]),
    booking(S, [NumberOfS,TimeforS,_,_,_,Menu]),
    TimeforN #= TimeforS,
    NumberOfN #= NumberOfS,
    Menu #= standard,
    NewTime is TimeforN + 2,
    format('Reservation can be booked for order number ~w at the time ~w hours.', [N, NewTime]).

%check if the time and table is equal with the previous orders. Also check the menu fromn the previous orders to get the value of the vacant time
%timeCheckWithPreviousOrderWithTime(?N, ?S)
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [NumberOfN,TimeforN,_,_,_,_]),
    booking(S, [NumberOfS,TimeforS,_,_,_,Menu]),
    TimeforN #= TimeforS,
    NumberOfN #= NumberOfS,
    Menu #= theatre,
    NewTime is TimeforN + 1,
    format('Reservation can be booked for order number ~w at the time ~w hours.', [N, NewTime]).    

%check if the time and table is equal with the previous orders. Also check the menu fromn the previous orders to get the value of the vacant time
%timeCheckWithPreviousOrderWithTime(?N, ?S)
timeCheckWithPreviousOrderWithTime(N, S):- 
    N #> 1,
    booking(N, [NumberOfN,TimeforN,_,_,_,_]),
    booking(S, [NumberOfS,TimeforS,_,_,_,Menu]),
    TimeforN #= TimeforS,
    NumberOfN #= NumberOfS,
    Menu #\= theatre,
    Menu #\= standard,
    NewTime is TimeforN + 2,
    format('Reservation can be booked for order number ~w at the time ~w hours.', [N, NewTime]),
    T is S - 1,
    timeCheckWithPreviousOrderWithTime(N, T).


%bookAccordingToTime(?N)
%Booking should be made with in the time 19:00 to 23:00
bookAccordingToTime(N):-
    booking(N, [_,TimeHour,_,_,_,_]),
    TimeHour #> 18,
    TimeHour #< 24.

%predicate to define the time for restaurant time to remain open
bookAccordingToTime(N):-
    booking(N, [_,TimeHour,_,_,_,_]),
    TimeHour #< 18,
    TimeHour #> 24.

%Table book for 1-2 people
bookAccordingToPeople(N):- 
    booking(N, [NumberOfPerson,_,_,_,_, _]),
    NumberOfPerson #> 0,
    NumberOfPerson #< 3.

%Table book for 3 people
bookAccordingToPeople(N) :-
    booking(N, [NumberOfPerson,_,_,_,_, _]),
    NumberOfPerson #= 3.

%Table book for 4 people
bookAccordingToPeople(N) :-
    booking(N, [NumberOfPerson,_,_,_,_, _]),
    NumberOfPerson #= 4.

%cannotBook(?N)
%Table book for more than 4 people
%Used cut here to stop backtracking. If there are more than 4 people, no way the restaurant can do something for them.
cannotBook(N) :-
    booking(N, [NumberOfPerson,_,_,_,_, _]),
    NumberOfPerson #> 4,
    format( ' Reserve cannot be made for booking number ~w for having more than 4 people .\n', [N]),
    !. 

%All the bookings are converted to 1-7. Putting any value from 1-7 in N will show the result.
%PUT NUMBERS HERE FROM 1-7 TO TEST
test_dcg(N, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]) :- booking(N, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]).

%succeeds if its first argument is an input phrase in the above format, its second argument is whatever our DCG returns to the program
%PUT NUMBERS HERE FROM 1-7 TO TEST
test_constraints(N, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]) :- booking(N, [NumberOfPerson,TimeHour,TimeMinute,Day,Month,Menu]), doSchedule(N).

%succeeds, running the entire program on each of the test sentences above and printing out the solutions
testall :- booking(N, [_,_,_,_,_,_]), doSchedule(N).



%Mishkat Haider Chowdhury
%ID:0594966