-module(murmur3).
-author('Damodharan Rajalingam').
-export([ hash32/2 ,hash32/1 ]).

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
    
hash32_tail_mix(K1, Hash) -> Hash bxor hash32_mmix(K1).

% 4-byte blocks
hash32_body(<<K1:32/little, Rest/binary>>, Hash) -> 
    K2 = hash32_mmix(K1),
    Hash2 = Hash bxor K2,
    Hash3 = rotl32(Hash2, 13),
    Hash4 = (Hash3 * 5 + 16#e6546b64) band ?MASK32,
    hash32_body(Rest, Hash4);
% Tail of the data (1-,2-,3- byte blocks)
hash32_body(<<K1:8>>, Hash) -> hash32_tail_mix(K1, Hash);
hash32_body(<<K1:16/little>>, Hash) -> hash32_tail_mix(K1, Hash);
hash32_body(<<K1:24/little>>, Hash) -> hash32_tail_mix(K1, Hash);
hash32_body(<<>>, Hash) -> Hash.

hash32_impl(Data, Seed) ->
    Len = byte_size(Data),
    hash32_fmix(hash32_body(Data, Seed) bxor Len).

hash32(Data, Seed) when is_binary(Data) -> hash32_impl(Data, Seed);
hash32(Data, Seed) -> hash32(term_to_binary(Data), Seed).

hash32(Data) -> hash32(Data, 16#BADDEED).
