# roller

*roller* is hot framework. It's just simple tool to control flow. I'm using 
*roller* for my web projects. 

*roller* is based on ideas of [kaph](https://github.com/akaspin/kaph). But 
instead *kaph*, *roller* not bound to any framework. With roller flow divides 
to chain of operations. Each operation takes result of previous.
 
## Usage

For example, a small server on [mochiweb](https://github.com/mochi/mochiweb) 
with following logic:

* Any `GET /slug/<param>/<param>/...` returns `200` with 
  "param-param-param...". Else returns `403`.
* Any `/backend/...` returns `200` with "backend".
* Any `/backend` `POST` requests must be authentificated (by cookie).
* Any other request returns `404`. 

Ok. Good design. Let's go.

Inside mochiweb loop module...

    ...
    mochiweb_http:start([{name, ?MODULE}, {port, Port}, 
                         {loop, fun (Request) ->
                                        accept(Request)
                                end}]),
    ...
    
    accept_request(Request) ->
        Roller = roller:new({Request, Request:get(method)}, 
                            fun(Code, Reason) ->
                                    handle_error(Request, Code, Reason)
                            end),
        case string:tokens(Request:get(path), "/") of
            ["slug" | Params] ->
                Roller:roll(Params, [op_slug, op_respond]);
            ["backend" | Params ] ->
                Roller:roll(Params, [op_auth, op_backend, op_respond]);
            [_] ->
                Roller:roll(null, [op_notfound])
        end.
    
    %% @doc Error handler
    handle_error(Request, Code, Reason) ->
        error_logger:error_report(["Roller flow error", 
                                   {code, Code}, {reason, Reason}]),
        Request:respond({Code, [], 
                        mochifmt:format("{0} : {1}", [Code, Reason])}).

op_auth.erl

    -module(op_auth).
    -export([do/2]).
    
    do({Request, 'POST'}, Params) ->
        case Request:get_cookie_value("__user") of
            undefined -> 
                % No cookie - no access
                throw({403, "Forbidden"});
            Cookie -> 
                {ok, Cookie, Params}
    end; 
    do(_, Params) ->
        {ok, Params}.
        
op_backend.erl
    
    -module(op_backend).
    -export([do/2]).
    
    op(_, _) ->
        {ok, "backend"}.
        
op_slug.erl

    -module(op_slug).
    -export([do/2]).
    
    op({Request, 'GET'}, Params) ->
        {ok, string:join(Params, "-")};
    op({Request, _}, _) ->
         throw({403, "Forbidden"}).  
         
op_notfound.erl
    
    -module(op_backend).
    -export([do/2]).
    
    op(_, _) ->
        throw({403, "Forbidden"}).

op_respond.erl
    
    -module(op_backend).
    -export([do/2]).
    
    op({Request}, {ok, Data}) ->
        Request:respond({200, [], Data}),
        finish.

As we see, now request "controllers" divided into small "ops". And they can be 
combined. 

    -type err_fun() :: fun((Code::integer(), Reason::any()) -> any()).
    -spec roller:new(Slug::any(), 
                     Error::err_fun()) -> {roller, any(), err_fun()}.
    -spec roll(any(), [atom()]) -> ok.
    
`roller:new` fun takes two arguments. First is any data what will be sent to 
each `<operation>:do` function. Second is error handling function which in 
turn takes error code and reason.




