-module(redbloom).
-export([new/3, new/4, 
         close/1,
         add/2, contains/2
        ]).

-include("redbloom.hrl").

-define(SALT_MAX, 4294967295).

new(Name, ElemCount, FPP) -> new(Name, ElemCount, FPP, [{host, "localhost"}, {port, 6379}]).

new(Name, ElemCount, FPP, _ROpt) ->
    Params = get_filter_params(ElemCount, FPP),
    {ok, RClient} = eredis:start_link(_ROpt),
    Filter = #bfilter{name=Name, mkey=Name++":meta", dkey=Name++":data",rclient=RClient,params=Params},
    init_redis_key(Filter).

add(#bfilter{rclient=C,dkey=Key,params=#bfilter_params{salts=Salts,tsize=M}}, Item) ->
    Bits = calc_bits(Item, Salts, M),
    set_bits(C, Key, Bits).

contains(#bfilter{rclient=C,dkey=Key,params=#bfilter_params{salts=Salts,tsize=M}}, Item) ->
    Bits = calc_bits(Item, Salts, M),
    check_bits(C, Key, Bits).

close(#bfilter{rclient=C}) ->
    eredis:stop(C).

calc_bits(Item, Salts, M) ->
    lists:map(fun (X) -> murmur3:hash32(Item, X) rem M end, Salts).

set_bits(C, Key, Bits) ->
    Res = eredis:qp(C, [ ["SETBIT", Key, N, 1] || N <- Bits ]),
    lists:foldl(fun ({ok, _}, Acc) -> Acc and true; (_, _) -> false end, true, Res).

check_bits(C, Key, Bits) ->
    Res = eredis:qp(C, [ ["GETBIT", Key, N]  || N <- Bits]),
    lists:foldl(fun ({ok, <<"1">>}, Acc) -> Acc and true; (_, _) -> false end, true, Res).

init_redis_key(F) when is_record(F, bfilter) -> 
    {ok,Exists} = eredis:q(F#bfilter.rclient, ["SETNX", F#bfilter.mkey, term_to_binary(F#bfilter.params)]),
    case init_redis_data_key(F, Exists) of
        ok -> {ok, F};
        {error, Reason} ->
            eredis:stop(F#bfilter.rclient),
            {error, Reason}
    end.

init_redis_data_key(F, <<"1">>) -> % filter does not exist in redis
    {ok, _} = eredis:q(F#bfilter.rclient, ["MULTI"]),
    {ok, _} = eredis:q(F#bfilter.rclient, ["SET", F#bfilter.dkey, 0]),
    {ok, _} = eredis:q(F#bfilter.rclient, ["SETBIT", F#bfilter.dkey, F#bfilter.params#bfilter_params.tsize, "0"]),
    {ok, _} = eredis:q(F#bfilter.rclient, ["EXEC"]),
    ok;
init_redis_data_key(F, <<"0">>) -> % filter exists in redis. check if it has same parameters
    Fparams = F#bfilter.params,
    {ok, <<Params_bin/binary>>} = eredis:q(F#bfilter.rclient, ["GET", F#bfilter.mkey]),
    Params = try
        binary_to_term(Params_bin) 
    catch
        _:_ -> {error, "Key already exists in redis and has different value"}
    end,
    case Params of
        #bfilter_params{tsize=_,salts=_} -> 
            if 
                Fparams == Params -> ok;
                true -> {error, "Filter already exists but has different parameters"}
            end;
        _ -> {error, "Key already exists in redis and has different value"}
    end.

get_filter_params(ElemCount, FPP) ->
    {TableSize, NumHashes} = find_optimal_params(ElemCount, FPP),
    Salts = find_salts(NumHashes),
    #bfilter_params{tsize=TableSize, salts=Salts}.

find_optimal_params(ElemCount, FPP) ->
    lists:min(lists:map(fun (K) -> { calc_table_size(ElemCount,FPP,K), K} end, lists:seq(1,1000))).

calc_table_size(ElemCount, FPP, K) -> trunc((- K * ElemCount) / math:log(1.0 - math:pow(FPP, 1.0 / K))) + 1.

find_salts(Num) -> 
    tl(lists:seq(1, ?SALT_MAX, ?SALT_MAX div Num)).
