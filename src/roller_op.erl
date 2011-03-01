%% @doc Behaviour for roller operation
-module(roller_op).
-behaviour(behaviour).
-export([behaviour_info/1]).

behaviour_info(callbacks)->
    [{do, 1}].
