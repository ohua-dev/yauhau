package com.ohua.fetch.operators;

import com.ohua.fetch.Request;
import com.ohua.fetch.RequestTree;
import com.ohua.fetch.RequestTreeLeaf;
import com.ohua.lang.Function;

/**
 * Created by justusadam on 01/03/16.
 */
public class MkReqLeaf {
    @Function
    public RequestTree __mkReqTreeLeaf(Request request) {
        return new RequestTreeLeaf(request);
    }
}
