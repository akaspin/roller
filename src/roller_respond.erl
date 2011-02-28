-module(roller_respond, [Request]).

-export([do/1]).

% @doc Send response or non-modified.
do({ok, equal})->
    send(not_modified),
    finish;
do({ok, Raw})->
    {Code, Headers, Data} = trans_response(Raw),
    Request:respond({Code, Headers, Data}),
    finish.

send(not_modified)->
    Request:respond({304, [get_server_header()], []}).

%% @doc Transverse flat response list to `{Code, Reason, Data}'.
trans_response(Raw)->
    Data = mochilists:get_value(<<"data">>, Raw),
    Code = list_to_integer(binary_to_list(
                             mochilists:get_value(<<"code">>,Raw,<<"200">>))),
    Headers = [{H, V} || {<<"header:", H/binary>>, V} <- Raw ],
    {Code, make_headers(Headers), Data}.

%% @doc Make response headers. Add `Content-Type' if needed and `Server'.
make_headers(Headers)->
    CType = case mochilists:is_defined(<<"Content-Type">>, Headers) of
        true -> [];
        _ -> [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}]
    end,
    lists:append([Headers, CType, [get_server_header()]]).

%% @doc Make server header. 
get_server_header()->
    {ok, [{description, Desc},_, {vsn, Vsn}|_]} = application:get_all_key(),
    {<<"Server">>, Desc++"/"++Vsn}.
