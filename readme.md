# roller

*roller* is hot franework. It's just simple tool to control flow inside 
[mochiweb](https://github.com/mochi/mochiweb) request.

## Design

*roller* is based on ideas of [kaph](https://github.com/akaspin/kaph). Request 
divides into chain of separate operations. 

first.erl
    
    -module(first, [Request]).
    -export([do/1]).
    
    do(["one"])->
        "URL One";
    do(["two"])->
        "URL Two";
    do(_)->
        "404".
        
second.erl

    -module(second, [Request]).
    -export([do/1]).
    
    do({ok, Data})->
        Request:respond({200, [], Data}),
        finish.
        
roll.erl

    ...
    % in mochiweb loop fun...
    Roller = roller:new(Request),
    Roller:roll(string:tokens(Request:get(path), "/"), 
                [first, second]).
    ...

Each operation is parameterized module that contains a clause `do/1`. Result 
of 