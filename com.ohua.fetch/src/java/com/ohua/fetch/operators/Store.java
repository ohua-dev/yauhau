package com.ohua.fetch.operators;

import com.ohua.fetch.Request;
import com.ohua.lang.Function;

import java.util.Collections;

/**
 * Created by sertel on 3/3/16.
 */
public class Store<P, R> {
    @Function
    public R store(Request<P, R> request) {
        Iterable<R> resp = request.getDataSource().store(Collections.singletonList(request.getPayload()));
        return resp.iterator().next();
    }
}
