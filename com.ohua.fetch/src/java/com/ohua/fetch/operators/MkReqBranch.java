package com.ohua.fetch.operators;

import com.ohua.fetch.RequestTree;
import com.ohua.fetch.RequestTreeBranch;
import com.ohua.lang.Function;

/**
 * Created by justusadam on 26/02/16.
 */
public class MkReqBranch {
    @Function
    public RequestTreeBranch __mkReqBranch(Iterable<RequestTree> trees) {
        return new RequestTreeBranch(trees);
    }
}
