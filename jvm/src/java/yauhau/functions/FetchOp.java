/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import ohua.lang.defsfn;
import yauhau.Request;

import java.util.ArrayList;

/**
 * Created by justusadam on 12/02/16.
 *
 * Implementation for a basic fetch operation.
 *
 * If the batching optimization is enabled this will never be called.
 */
public final class FetchOp<P, R> {
    @defsfn
    public R fetch(Request<P, R> request) {
        ArrayList<P> l = new ArrayList<>();
        l.add(request.getPayload());
        Iterable<R> res = request.getDataSource().fetch(l);
        return res.iterator().next();
    }
}
