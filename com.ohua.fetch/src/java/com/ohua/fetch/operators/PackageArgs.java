package com.ohua.fetch.operators;

import com.ohua.lang.Function;

/**
 * Created by justusadam on 29/02/16.
 */
public class PackageArgs {
    @Function
    public Object[] __packageArgs(Object... args) {
        return args;
    }
}
