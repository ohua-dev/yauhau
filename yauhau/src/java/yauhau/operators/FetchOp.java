/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.Request;

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