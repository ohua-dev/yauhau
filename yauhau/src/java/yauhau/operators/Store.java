/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.Request;

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
