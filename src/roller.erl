-module(roller, [Slug, Error]).

-export([new/1, new/2]).
-export([roll/2]).

-export_type([err_fun/0]).

-type err_fun() :: fun(
    (Class :: throw | exit | error, 
     Code :: integer(),
     Reason :: any()) -> any()).    % Error handling function. 

%% @doc Constructor 
-spec new(Slug::any(), Error::err_fun()) -> {roller, any(), err_fun()}.
new(Slug, Error)->
    instance(Slug, Error).

%% @doc Constructor 
new(Slug)->
    Err = fun(_Class, Code, Reason)-> 
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
            Error(throw, Code, Reason);
        error:Reason->
            Error(error, 500, {Reason, erlang:get_stacktrace()});
        Class:Reason -> 
            Error(Class, 500, Reason)
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

