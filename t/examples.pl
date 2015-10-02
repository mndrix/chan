:- use_module(library(chan), []).

say_hi(Tx) :-
    chan:send(Tx, hi),
    chan:close(Tx).

chatty(Tx) :-
    forall(member(Word,[hello,my,conversation,pal]),chan:send(Tx,Word)),
    chan:close(Tx).

:- dynamic work/1.
worker(Rx) :-  % assert terms sent to it
    chan:recv(Rx,Term),
    !,
    assertz(work(Term)),
    worker(Rx).
worker(_).

thread_exists(Thread) :-
    catch(thread_property(Thread,status(_)),_,fail).


:- use_module(library(tap)).

single_thread :-
    chan:new(Tx,Rx),
    chan:send(Tx,hello),
    chan:recv(Rx,Message),
    Message == hello.


two_threads :-
    chan:new(Tx,Rx),
    thread_create(say_hi(Tx),_,[detached(true)]),
    chan:recv(Rx,Message),
    Message == hi,
    \+ chan:recv(Rx,_).  % channel is closed; can't receive more


multiple_messages :-
    chan:new(Tx,Rx),
    thread_create(chatty(Tx),_,[detached(true)]),
    findall(Word,chan:recvd(Rx,Word),Words),
    Words == [ hello, my, conversation, pal ].


workers :-
    % create worker threads
    chan:new(Tx,Rx),
    thread_create(worker(Rx),A,[detached(true)]),
    thread_create(worker(Rx),B,[detached(true)]),

    % send a bunch of work
    chan:send(Tx,one),
    chan:send(Tx,two),
    chan:send(Tx,three),
    chan:close(Tx),

    % verify that the work was completed
    sleep(0.1),  % give workers time to finish
    work(one),
    work(two),
    work(three),

    % verify that workers are gone
    sleep(0.1), % give workers time to clean up
    \+ thread_exists(A),
    \+ thread_exists(B).


double_close(throws(not_allowed_on_channel(close,tx_closed))) :-
    chan:new(Tx,_),
    chan:close(Tx),
    chan:close(Tx).

close_wrong_end(throws(not_allowed_on_channel(close,rx))) :-
    chan:new(_,Rx),
    chan:close(Rx).

send_wrong_end(throws(not_allowed_on_channel(send,rx))) :-
    chan:new(_,Rx),
    chan:send(Rx).

recv_wrong_end(throws(not_allowed_on_channel(recv,tx))) :-
    chan:new(Tx,_),
    chan:recv(Tx).
