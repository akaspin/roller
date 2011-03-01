# roller

*roller* is hot franework. It's just simple tool to control flow inside 
[mochiweb](https://github.com/mochi/mochiweb) request.

## Design

*roller* is based on ideas of [kaph](https://github.com/akaspin/kaph). Request 
divides into chain of separate operations. 

first.erl:
    
    -module(first, [Request]).
    -behaviour(roller_op).
    -export([do/1]).
    
    do(["one"])->
        {ok, "URL One"};
    do(["two"])->
        {ok, "URL Two"};
    do(_)->
        {not_found}.
        
second.erl:

    -module(second, [Request]).
    -behaviour(roller_op).
    -export([do/1]).
    
    do({ok, Data})->
        Request:respond({200, [], Data}),
        finish.
    do({not_found})->
        throw({404, "Page not found"}).
        
roll.erl:

    ...
    % in mochiweb loop fun...
    Roller = roller:new(Request),
    Roller:roll(string:tokens(Request:get(path), "/"), 
                [first, second]).
    ...

Each operation is parameterized module that contains a clause `do/1`. Result 
of the `do/1` transferred between the following operation. For greater 
convenience when writing modules of operations you can use `roller_op` 
behaviour.

    -spec do(any()) -> any() | finish.

If operation returns `finish` - request processing stops. All errors will be 
hanled, logged and, if possible, sent to the client. 

Constructor of roller `roller:new/1` takes one argument - `mochiweb_request` 
instance. Function `roll/2` of instance of `roller` takes two arguments. First 
is `any()` argument of first `do/1` in chain. Second is list of atoms, each of 
which denotes the operation module in the chain.

    spec new(MochiWebRequest) -> RollerInstance.
    spec roll(any(), [atom()]) - ok.

### Error handling

As written earlier, *roller* handles any exceptions within operations. If you 
need to throw your exception, do it in manner `throw({integer(), any()})`. 
First term of the tuple is HTTP code. Second is reason.

    throw({404, "Page not found"})

All other exceptions will be handled as `{500, any()}`. Where `500` corresponds 
to the HTTP `Internal server error`. Second term in tuple is exception data.

### Finish him!

If none of the operations in the chain has not returned "finish", the "roller" 
will throw an exception `{500, {"Non ended chain", any()}}`. Where the second 
term of the tuple containing the exception data is the data returned by the 
last function `do/1` in the chain.

### Custom error report

By default, error messages are quite informative, but not very aesthetic. 
You can fix this by using another constructor `roller:new/2`, specifying the 
second parameter as `fun/2` that generates error message.

    type err_fun::fun( (Code::integer(), Reason::any())->
                   {integer(), ioheaders(), iodata() | {file, IoDevice}}) 
    spec new(MochiWebRequest, ErrFun::err_fun) -> RollerInstance.
    
    Roller = roller:new(Request, fun(Code, Reason)-> 
                {Code, [], mochifmt:format("{0} : {1}", [Code, Reason])}
            end).

This `fun/2` must return tuple that is used at parameter of non-chunked 
`mochiweb_request:respond/1`.

## 

## Why use parametrized modules?

The reason is speed. Implementation with parametrized modules is faster by 
10% than usual approach. I don't know why.


    