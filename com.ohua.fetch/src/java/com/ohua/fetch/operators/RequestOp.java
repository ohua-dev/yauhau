package com.ohua.fetch.operators;

import com.ohua.fetch.IDataSource;
import com.ohua.fetch.Request;
import com.ohua.fetch.RequestTreeLeaf;

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
