# redbloom - Bloom Filter with Redis backend
This is an erlang library which implements Bloom filter with redis as backend data store to store the bloom filter

```erlang
{ok, Filter} = redbloom:new("myfilter",10000000,0.1,[{host,localhost}]).
redbloom:add(Filter, "item1").
redbloom:add(Filter, "item2").
redbloom:contains(Filter, "item1").
redbloom:contains(Filter, "item3").
redbloom:close(Filter).
```




