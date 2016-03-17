package com.ohua.fetch.operators;

import com.ohua.lang.Function;

/**
 * Created by justusadam on 02/03/16.
 */
public class Identity {
    @Function
    public Object identity(Object input) {
        return input;
    }
}
