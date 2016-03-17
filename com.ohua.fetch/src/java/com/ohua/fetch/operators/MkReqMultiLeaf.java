package com.ohua.fetch.operators;

import com.ohua.fetch.Request;
import com.ohua.fetch.RequestTreeMultiLeaf;
import com.ohua.lang.Function;

/**
 * Created by justusadam on 26/02/16.
 */
public class MkReqMultiLeaf {
    @Function
    public RequestTreeMultiLeaf __mkReqMultiLeaf(Iterable<Request> requests) {
        return new RequestTreeMultiLeaf(requests);
    }
}
