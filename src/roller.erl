-module(roller, [Request, Error]).

-export([new/1, new/2]).
-export([roll/2]).

new(Request, Error)->
    instance(Request, Error).
new(Request)->
    Err = fun(Code, Reason, Class)-> 
                  {Code, 
                   [{<<"Content-Type">>,<<"text/plain; charset=utf-8">>}], 
                   mochifmt:bformat(
                    "{0} ({1}): {2}", [Code, Class, Reason])}
          end,
    instance(Request, Err).


roll(Args, Chain)->
    do(Args, Chain).

do(Args, [])->
    case Args of
        ok -> 
            ok;
        NonClosed -> 
            % Some wrong here
            send_error(500, {"Non ended chain", NonClosed}, "Error")
    end;

do(Args, [Current|Rest])->
    Handler = Current:new(Request),
    try Handler:do(Args) of
        ok -> ok;
        Ret -> do(Ret, Rest)
    catch
        Class:{Code, Reason} when is_integer(Code)->
            send_error(Code, Reason, Class);
        Class:Err->
            Rem = integer_to_list(length(Rest)),
            Mod = atom_to_list(Current),
            send_error(500, {"Error in '" ++ Mod ++ "'. " ++ 
            Rem ++ " steps left.", Err}, Class)
    end.

send_error(Code, Reason, Class)->
    error_logger:error_report(["Roller flow error", {class, Class},
                               {code, Code}, {reason, Reason}]),
    Request:respond(Error(Code, Reason, Class)).