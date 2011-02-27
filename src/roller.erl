-module(roller, [Request, Error]).

-export([new/1, new/2]).
-export([roll/2]).

new(Request, Error)->
    instance(Request, Error).
new(Request)->
    Err = fun(Code, Reason, Class)-> 
                  {Code, 
                   [{<<"Content-Type">>,<<"text/plain; charset=utf-8">>}], 
                   mochifmt:format(
                    "{0} ({1}): {2}", [Code, Class, Reason])}
          end,
    instance(Request, Err).


roll(Args, Chain)->
    do(Args, Chain, 0).

do(Args, [], _)->
    case Args of
        ok -> 
            ok;
        NonClosed -> 
            % Some wrong here
            send_error(500, {"Non ended chain", NonClosed}, "Error")
    end;

do(Args, [Current|Rest], Step)->
    Handler = Current:new(Request),
    try Handler:do(Args) of
        ok -> ok;
        Ret -> do(Ret, Rest, Step+1)
    catch
        throw:{Code, Reason} when is_integer(Code)->
            send_error(Code, {make_step_error(Step, Current), Reason}, throw);
        throw:Reason -> 
            send_error(500, {make_step_error(Step, Current), Reason}, throw);
        exit:Reason -> 
            send_error(500, {make_step_error(Step, Current), Reason}, 'EXIT');
        error:Reason->
            send_error(500, {make_step_error(Step, Current), 
                lists:append([Reason],erlang:get_stacktrace())}, error)
    end.

make_step_error(Step, Current)->
    N = integer_to_list(Step),
    Mod = atom_to_list(Current),
    "Error on step "++N++" in '"++Mod++"'.".

send_error(Code, Reason, Class)->
    error_logger:error_report(["Roller flow error", {class, Class},
                               {code, Code}, {reason, Reason}]),
    Request:respond(Error(Code, Reason, Class)).