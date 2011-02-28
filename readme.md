# roller

*roller* is hot franework. It's just simple tool to control flow inside 
[mochiweb](https://github.com/mochi/mochiweb) request.

## Design

*roller* is based on ideas of [kaph](https://github.com/akaspin/kaph). Request 
divides into chain of separate operations. 

first.erl:
    
    -module(first, [Request]).
    -export([do/1]).
    
    do(["one"])->
        {ok, "URL One"};
    do(["two"])->
        {ok, "URL Two"};
    do(_)->
        {not_found, "404 : Not found"}.
        
second.erl:

    -module(second, [Request]).
    -export([do/1]).
    
    do({ok, Data})->
        Request:respond({200, [], Data}),
        finish.
    do({not_found, Data})->
        throw({404, Data}).
        
roll.erl:

    ...
    % in mochiweb loop fun...
    Roller = roller:new(Request),
    Roller:roll(string:tokens(Request:get(path), "/"), 
                [first, second]).
    ...

Each operation is parameterized module that contains a clause `do/1`. Result 
of the `do/1` transferred between the following operation. If operation 
returns `finish` - request processing stops. 