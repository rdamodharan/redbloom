-module(murmur3).
-author('Damodharan Rajalingam').
-export([ hash32/2 ]).

-define(C1, 16#cc9e2d51).
-define(C2, 16#1b873593).
-define(MASK32, 16#ffffffff).

rotl32(Num, R) -> ((Num bsl R) bor (Num bsr (32 - R))) band ?MASK32.

hash32_mmix(K1) -> 
    K2 = (K1 * ?C1) band ?MASK32,
    K3 = rotl32(K2,15),
    (K3 * ?C2) band ?MASK32.

hash32_fmix(H) -> 
    H2 = ((H bxor (H bsr 16)) * 16#85ebca6b) band ?MASK32,
    H3 = ((H2 bxor (H2 bsr 13)) * 16#c2b2ae35) band ?MASK32,
    H3 bxor (H3 bsr 16).
    
hash32_body(<<A:8,B:8,C:8,D:8, Rest/binary>>, Hash) -> 
    K1 = (D bsl 24) bor (C bsl 16) bor (B bsl 8) bor A,
%    io:format("Key block: ~w, Rest: ~w~n", [K1, Rest]),
    K2 = hash32_mmix(K1),
    Hash2 = Hash bxor K2,
    Hash3 = rotl32(Hash2, 13),
    Hash4 = (Hash3 * 5 + 16#e6546b64) band ?MASK32,
    hash32_body(Rest, Hash4);

hash32_body(Data,Hash) -> 
%    io:format("At end of body: [~w, ~w]~n", [Data, Hash]),
    {Data, Hash}.


hash32_tail(Tail, Hash) -> 
    K1 = lists:foldr(fun(X, Acc) -> (Acc bsl 8) bxor X end, 0, binary_to_list(Tail)),
    Hash bxor hash32_mmix(K1).

hash32_impl(Data, Seed) ->
    Len = byte_size(Data),
    {Data2, Hash2} = hash32_body(Data, Seed),
%    io:format("After body calc: ~w~n",[Hash2]),
    Hash3 = case Data2 of
            <<>> -> Hash2;
            Tail -> hash32_tail(Tail, Hash2)
        end,
%    io:format("After Tail calc: ~w~n",[Hash3]),
    Hash4 = Hash3 bxor Len,
%    io:format("After Len xor: ~w~n",[Hash4]),
    Hash5 = hash32_fmix(Hash4),
%    io:format("After fmix: ~w~n",[Hash5]),
    Hash5.

hash32(Data, Seed) when is_binary(Data) -> hash32_impl(Data, Seed);
hash32(Data, Seed) -> hash32(term_to_binary(Data), Seed).
