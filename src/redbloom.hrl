-record(bfilter_params, {tsize, salts}).
-record(bfilter,{ name, mkey, dkey, rclient, params=#bfilter_params{} }).

