# roller

*roller* is hot framework. It's just simple tool to control flow inside 
[mochiweb](https://github.com/mochi/mochiweb) request.

## Design

*roller* is based on ideas of [kaph](https://github.com/akaspin/kaph). Request 
divides into chain of separate operations. 

first.erl:
    
    -module(first, [Request]).
    -behaviour(roller_op).
    -export([do/2]).
    
    do('GET', ["one"])->
        {ok, "URL One"};
    do('GET', ["two"])->
        {ok, "URL Two"};
    do(_, _)->
        {not_found}.
        
second.erl:

    -module(second, [Request]).
    -behaviour(roller_op).
    -export([do/2]).
    
    do(_, {ok, Data})->
        Request:respond({200, [], Data}),
        finish.
    do(_, {not_found})->
        throw({404, "Page not found"}).
        
roll.erl:

    ...
    % in mochiweb loop fun...
    Roller = roller:new(Request:get(method), Request),
    Roller:roll(string:tokens(Request:get(path), "/"), 
                [first, second]).
    ...

Constructor of roller `roller:new/2` takes two arguments:any data to be 
transferred to each operation in the chain and `mochiweb_request` 
instance. Function `roll/2` of instance of `roller` takes two arguments. First 
is `any()` argument of first `do/2` in chain. Second is list of atoms, each of 
which denotes the operation module in the chain.

    spec new(Slug::any(), Request) -> RollerInstance.
    spec roll(Args::any(), Chain::[atom()]) - ok.
    
Each operation is parameterized module that contains a clause `do/2`. First 
argument of `do/2` is `Slug::any()` (you've heard about it). Second argument 
is result of previous operation.  

    spec do(any(), any()) -> any() | finish.

For greater convenience when writing modules of operations you can use 
`roller_op` behaviour. It's optional.


If operation returns `finish` - request processing stops. All errors will be 
hanled, logged and, if possible, sent to the client. 

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
last function `do/2` in the chain.

### Custom error report

By default, error messages are quite informative, but not very aesthetic. 
You can fix this by using another constructor `roller:new/2`, specifying the 
second parameter as `fun/2` that generates error message.

    type err_fun::fun( (Code::integer(), Reason::any())->
                   {integer(), ioheaders(), iodata() | {file, IoDevice}}) 
    spec new(Slug::any(), Request, ErrFun::err_fun) -> RollerInstance.
    
    Roller = roller:new([], Request, fun(Code, Reason)-> 
                {Code, [], mochifmt:format("{0} : {1}", [Code, Reason])}
            end).

This `fun/2` must return tuple that is used at parameter of non-chunked 
`mochiweb_request:respond/1`.

## Why use parametrized modules?

The reason is speed. Implementation with parametrized modules is faster by 
10% than usual approach. I don't know why.


    