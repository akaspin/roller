-module(roller, [Slug, Error]).

-export([new/1, new/2]).
-export([roll/2]).

%% @doc Constructor 
new(Slug, Error)->
    instance(Slug, Error).

%% @doc Constructor 
new(Slug)->
    Err = fun(Code, Reason)-> 
                  error_logger:error_report(
                    ["Roller flow error", 
                     {code, Code}, {reason, Reason}])
          end,
    instance(Slug, Err).

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
    case Current:do(Slug, Args) of
        finish -> ok;
        Ret -> do(Ret, Rest)
    end.

send_error(Code, Reason)->
    Error(Code, Reason).