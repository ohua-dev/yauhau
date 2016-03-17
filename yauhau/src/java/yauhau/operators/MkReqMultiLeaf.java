/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.Request;
import yauhau.RequestTreeMultiLeaf;

/**
 * Created by justusadam on 26/02/16.
 */
public class MkReqMultiLeaf {
    @Function
    public RequestTreeMultiLeaf __mkReqMultiLeaf(Iterable<Request> requests) {
        return new RequestTreeMultiLeaf(requests);
    }
}
