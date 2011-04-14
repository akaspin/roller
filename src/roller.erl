-module(roller, [Slug, Error]).

-export([new/1, new/2]).
-export([roll/2]).

-type err_fun() :: fun((Code::integer(), Reason::any()) -> any()).

%% @doc Constructor 
-spec new(Slug::any(), Error::err_fun()) -> {roller, any(), err_fun()}.
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
-spec roll(Args::any(), Chain::[atom()]) -> ok.
roll(Args, Chain)->
    try
        do(Args, Chain)
    catch
        throw:{Code, Reason} when is_integer(Code)->
            Error(Code, Reason);
        throw:Reason -> 
            Error(500, Reason);
        exit:Reason -> 
            Error(500, Reason);
        error:Reason->
            Error(500, {Reason, erlang:get_stacktrace()})
    end.

%% @doc End of chain 
do(Args, [])->
    case Args of
        finish -> 
            ok;
        NonClosed -> 
            % Some wrong here
            throw({500, {"Non ended chain", NonClosed}})
    end;

%% @doc Execute chain op 
do(Args, [Current|Rest])->
    case Current:do(Slug, Args) of
        finish -> ok;
        Ret -> do(Ret, Rest)
    end.

