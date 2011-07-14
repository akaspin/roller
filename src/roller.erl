-module(roller, [Slug, ErrMod]).

-export([new/1, new/2]).
-export([roll/2]).
-export([oops/2]).

-export_type([roller/0, err_mod/0]).

-type roller() :: {roller, any(), err_mod()}.
% Roller instance.

-type err_mod() :: module().
% Error handling callback module.

%% @doc Constructor 
-spec new(Slug, ErrMod) -> Instance when
        Slug :: any(),
        ErrMod :: err_mod(),
        Instance :: roller().

new(Slug, ErrMod)->
    instance(Slug, ErrMod).

%% @doc Constructor 
new(Slug)->
    instance(Slug, roller).

%% @doc Process request chain 
-spec roll(Args::any(), Chain::[atom()]) -> ok.
roll(Args, Chain)->
    try
        do(Args, Chain)
    catch
        Class:Reason -> 
            ErrMod:oops(Class, Reason)
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


%% @doc Standart error handling function.
-spec oops(Class, Reason) -> none() when 
        Class :: throw | error | exit,
        Reason :: any().
oops(Class, Reason) ->
    error_logger:error_report(["Roller flow error", {Class, Reason}]).

