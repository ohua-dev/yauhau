/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import yauhau.IDataSource;
import yauhau.Request;
import yauhau.RequestTreeLeaf;

/**
 * Created by justusadam on 01/03/16.
 */
public class RequestOp {
    public RequestTreeLeaf request(Object payload, IDataSource dataSource) {
        @SuppressWarnings("unchecked")
        Request r = new Request(payload, dataSource);
        return new RequestTreeLeaf(r);
    }
}
