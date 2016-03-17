/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.Request;
import yauhau.RequestTree;
import yauhau.RequestTreeLeaf;

/**
 * Created by justusadam on 01/03/16.
 */
public class MkReqLeaf {
    @Function
    public RequestTree __mkReqTreeLeaf(Request request) {
        return new RequestTreeLeaf(request);
    }
}
