/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.RequestTree;
import yauhau.RequestTreeBranch;

/**
 * Created by justusadam on 26/02/16.
 */
public class MkReqBranch {
    @Function
    public RequestTreeBranch __mkReqBranch(Iterable<RequestTree> trees) {
        return new RequestTreeBranch(trees);
    }
}
