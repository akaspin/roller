-module(roller, [Error]).

-export([new/0, new/1]).
-export([roll/3]).

new(Error)->
    instance(Error).
new()->
    Err = fun(Code, Reason)-> 
                  {Code, 
                   [<<"Content-Type">>,<<"text/plain; charset=utf-8">>], 
                   mochifmt:bformat("{0}\n{1}", [Code, Reason])}
          end,
    instance(Err).


roll(Request, Args, Chain)->
    do(Request, Args, Chain).

do(Request, Args, [])->
   case Args of
       ok -> 
           ok;
       Err -> 
           % Some wrong here
           send_error(Request, 500, {"End of chain", Err})
   end;

do(Request, Args, [Current|Rest])->
    Handler = Current:new(Request),
    try Handler:do(Args) of
        ok -> ok;
        Ret -> do(Request, Ret, Rest)
    catch
        error:{Code, Reason}->
            send_error(Request, Code, Reason);
            
        Class:Err->
            Rem = integer_to_list(length(Rest)),
            send_error(Request, 500, 
                       {"Error. " ++ Rem ++ " steps left.", {Class, Err}})
    end.

send_error(Request, Code, Reason)->
    Request:respond(Error(Code, Reason)).