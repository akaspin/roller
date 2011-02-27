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
    Chain = [first, second],
    Roller = roller:new(Request),
    ...
    % in loop fun...
        Roller:roll("Arg", Chain).
