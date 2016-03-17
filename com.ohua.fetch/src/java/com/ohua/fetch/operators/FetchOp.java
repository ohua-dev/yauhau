package com.ohua.fetch.operators;

import com.ohua.fetch.Request;
import com.ohua.lang.Function;

import java.util.ArrayList;

/**
 * Created by justusadam on 12/02/16.
 */
public final class FetchOp<P, R> {
    @Function
    public R fetch(Request<P, R> request) {
        ArrayList<P> l = new ArrayList<>();
        l.add(request.getPayload());
        Iterable<R> res = request.getDataSource().fetch(l);
        return res.iterator().next();
    }
}
