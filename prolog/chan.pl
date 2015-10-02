:- module(chan, [new/2, send/2, recv/2, recvd/2, close/1]).

:- use_module(library(error),[]).

% define types for library(error), etc.
:- multifile error:has_type/2.
error:has_type(chan,Chan) :-
    once( error:has_type(tx_chan,Chan)
        ; error:has_type(rx_chan,Chan)
        ).
error:has_type(tx_chan,tx_chan(Q,Status)) :-
    is_message_queue(Q),
    ground(Status),
    memberchk(Status,[open,closed]).
error:has_type(rx_chan,rx_chan(Q)) :-
    is_message_queue(Q).


%% new(-Tx:tx_chan,-Rx:rx_chan) is det.
%
%  Create a new channel for transmitting (Tx) and receiving (Rx) terms. The
%  channel has capacity to hold one term at a time.
new(tx_chan(Q,open),rx_chan(Q)) :-
    message_queue_create(Q,[max_size(1)]).


%% send(+Tx:ch_chan,+Term) is det.
%
%  Send Term down the Tx channel. Blocks if the channel is full. Throws an
%  exception if called with a receive channel.
send(tx_chan(Q,Status),Term) :-
    ( Status=open ->
        thread_send_message(Q,msg(Term),[])
    ; Status=closed ->
        throw(not_allowed_on_channel(send,tx_closed))
    ).
send(rx_chan(_)) :-
    throw(not_allowed_on_channel(send,rx)).


%% recv(+Rx:rx_chan,+Term) is semidet.
%
%  Receive the next Term from an Rx channel.  Blocks until a term arrives.
%  Fails for a closed, receive channel. Throws an exception for a transmit
%  channel.
recv(rx_chan(Q),Term) :-
    catch(thread_get_message(Q,MaybeTerm),_,fail),
    ( MaybeTerm=close ->
        message_queue_destroy(Q),
        fail
    ; MaybeTerm=msg(Term) ->
        true
    ; otherwise ->
        throw(unexpected_channel_term(MaybeTerm))
    ).
recv(tx_chan(_,_)) :-
    throw(not_allowed_on_channel(recv,tx)).


%% recvd(+Rx:rx_chan,-Term) is multi.
%
%  True if Term is a message received on Rx. Iterates terms on backtracking
%  until Rx closes.
recvd(Rx,Term) :-
    ( recv(Rx,Term) -> true; !, fail ).
recvd(Rx,Term) :-
    recvd(Rx,Term).


%% close(+Tx:tx_chan) is det.
%
%  Close the Tx channel.  Closing an already closed channel throws an
%  exception.  Trying to close a receive channel throws an exception.
:- redefine_system_predicate(chan:close(_)).
close(Tx) :-
    Tx=tx_chan(Q,Status),  % for nb_setarg/3
    !,
    ( Status=open ->
        thread_send_message(Q,close),
        nb_setarg(2,Tx,closed)
    ; Status=closed ->
        throw(not_allowed_on_channel(close,tx_closed))
    ; otherwise ->
        throw(unexpected_channel_status(Status))
    ).
close(rx_chan(_)) :-
    throw(not_allowed_on_channel(close,rx)).


/*

not sure this API can make sense without a blocking message_queue_peek to handle
empty queues.

%% is_closed(+Chan:chan) is semidet.
%
%  True if Chan is closed.  Never blocks for a transmit channel.  May block
%  for an empty receive channel.
is_closed(tx_chan(_,closed)).
is_closed(rx_chan(Q)) :-
    catch(thread_get_message(Q,closed,[timeout(0)]),E,true),
    once( nonvar(E)                % queue is gone, so channel is closed
        ; message_queue_destroy(Q) % fetched 'closed', channel is now closed
        ).
*/


% true if Q is a message queue
is_message_queue(Q) :-
    ground(Q),
    catch(message_queue_property(Q,size(_)),_,fail).
