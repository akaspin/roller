# roller

first.erl
    
    -module(second, [Request]).
    -export([do/1]).
    
    do(arg)->
        Data = "SomeData",
        Data.
        
second.erl

    -module(second, [Request]).
    -export([do/1]).
    do(Data)->
        Request:respond({200, [], Data}),
        ok.
        
roll.erl

    ...
    % in loop fun...
    Roller = roller:new(Request),
    Roller:roll("Arg", [first, second]).
    ...
