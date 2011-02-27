# roller

first.erl
    
    do(arg)->
        ok.
        
second.erl

    do(ok)->
        stop.
        
roll.erl

    Chain = [first, second],
    Roller = roller:new(Request),

    % in loop fun...
        Roller:roll("Arg", Chain).
