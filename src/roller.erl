-module(roller, [Error]).

-export([new/0, new/1]).
-export([roll/3]).

new(Error)->
    instance(Error).
new()->
    Err = fun(Code, Reason, Class)-> 
                  {Code, 
                   [{<<"Content-Type">>,<<"text/plain; charset=utf-8">>}], 
                   mochifmt:bformat(
                    "{0} ({1}): {2}", [Code, Class, Reason])}
          end,
    instance(Err).


roll(Request, Args, Chain)->
    do(Request, Args, Chain).

do(Request, Args, [])->
    case Args of
        ok -> 
            ok;
        NonClosed -> 
            % Some wrong here
            send_error(Request, 500, {"Non ended chain", NonClosed}, "Error")
    end;

do(Request, Args, [Current|Rest])->
    Handler = Current:new(Request),
    try Handler:do(Args) of
        ok -> ok;
        Ret -> do(Request, Ret, Rest)
    catch
        Class:{Code, Reason} when is_integer(Code)->
            send_error(Request, Code, Reason, Class);
            
        Class:Err->
            Rem = integer_to_list(length(Rest)),
            send_error(Request, 500, 
                    {"Error. " ++ Rem ++ " steps left.", Err}, Class)
    end.

send_error(Request, Code, Reason, Class)->
    error_logger:error_report(["Roller flow error", {class, Class},
                               {code, Code}, {reason, Reason}]),
    Request:respond(Error(Code, Reason, Class)).