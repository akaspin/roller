%% @doc Behaviour for roller operation
-module(roller_op).
-export([behaviour_info/1]).

behaviour_info(callbacks)->
    [{new, 1},{do, 2}];
behaviour_info(_Other) -> 
    undefined. 
