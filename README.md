# Synopsis

```prolog
:- use_module(library(chan),[]).
?-  chan:new(Tx,Rx),
    chan:send(Tx, hello),
    chan:recv(Rx, Message).
Message = hello .
```

# Description

SWI-Prolog has great primitives for [communicating between threads](http://www.swi-prolog.org/pldoc/man?section=threadcom).  This library is for applications which prefer a directional channel abstraction similar to that used in Go or Rust.

This is a relatively thin API on top of SWI-Prolog's message queues.

# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(chan).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/chan
