-module(roller, [Request, Env, Chain, Error]).

-export([new/3, new/4]).
-export([roll/1]).

new(Request, Env, Chain, Error)->
    instance(Request, Env, Chain, Error).
new(Request, Env, Chain)->
    Err = fun(Code, Reason)-> 
                  {Code, 
                   [<<"Content-Type">>,<<"text/plain; charset=utf-8">>], 
                   mochifmt:bformat("{0}\n{1}", [Code, Reason])}
          end,
    instance(Request, Env, Chain, Err).


roll(Args)->
    do(Args, Chain).

do(Args, [])->
   case Args of
       stop -> 
           ok;
       Err -> 
           % Some wrong here
           send_error(500, {"End of chain", Err})
   end;

do(Args, [Current|Rest])->
    try Current:do(Args) of
        stop -> ok;
        Ret -> do(Ret, Rest)
    catch
        Class:Err->
            Nexus = integer_to_list(length(Chain) - length(Rest)),
            send_error(500, {"Error on nexus " ++ Nexus, {Class, Err}})
    end.

send_error(Code, Reason)->
    Request:respond(Error(Code, Reason)).