-module(roller, [Request, Error]).

-export([new/1, new/2]).
-export([roll/2]).

%% @doc Constructor 
new(Request, Error)->
    instance(Request, Error).

%% @doc Constructor 
new(Request)->
    Err = fun(Code, Reason)-> 
                  {Code, 
                   [{<<"Content-Type">>,<<"text/plain; charset=utf-8">>}], 
                   mochifmt:format("{0} ({1}): {2}", [Code, Reason])}
          end,
    instance(Request, Err).

%% @doc Process request chain 
-spec roll(any(), [atom()]) -> ok.
roll(Args, Chain)->
    try
        do(Args, Chain)
    catch
        throw:{Code, Reason} when is_integer(Code)->
            send_error(Code, Reason);
        throw:Reason -> 
            send_error(500, Reason);
        exit:Reason -> 
            send_error(500, Reason);
        error:Reason->
            send_error(500, 
                {Reason,erlang:get_stacktrace()})
    end.

do(Args, [])->
    case Args of
        finish -> 
            ok;
        NonClosed -> 
            % Some wrong here
            throw({500, {"Non ended chain", NonClosed}})
    end;

do(Args, [Current|Rest])->
    Handler = Current:new(Request),
    case Handler:do(Args) of
        finish -> ok;
        Ret -> do(Ret, Rest)
    end.

send_error(Code, Reason)->
    error_logger:error_report(["Roller flow error", 
                               {code, Code}, {reason, Reason}]),
    Request:respond(Error(Code, Reason)).