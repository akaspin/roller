-module(roller, [Request, Error]).

-export([new/1, new/2]).
-export([roll/2]).

new(Request, Error)->
    instance(Request, Error).
new(Request)->
    Err = fun(Code, Reason)-> 
                  {Code, 
                   [<<"Content-Type">>,<<"text/plain; charset=utf-8">>], 
                   mochifmt:bformat("{0}\n{1}", [Code, Reason])}
          end,
    instance(Request, Err).


roll(Args, Chain)->
    do(Args, Chain).

do(Args, [])->
   case Args of
       ok -> 
           ok;
       Err -> 
           % Some wrong here
           send_error(500, {"End of chain", Err})
   end;

do(Args, [Current|Rest])->
    Handler = Current:new(Request),
    try Handler:do(Args) of
        ok -> ok;
        Ret -> do(Ret, Rest)
    catch
        Class:Err->
            Rem = integer_to_list(length(Rest)),
            send_error(500, {"Error. " ++ Rem ++ " steps left.", 
                             {Class, Err}})
    end.

send_error(Code, Reason)->
    Request:respond(Error(Code, Reason)).