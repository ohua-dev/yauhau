/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import com.ohua.lang.compile.analysis.qual.ReadOnly;
import yauhau.*;

/**
 * Created by justusadam on 17/03/16.
 */
public class UtilityOperators {

    public static final class Identity {
        @Function
        public Object identity(@ReadOnly Object input) {
            return input;
        }
    }

    public static final class MkReqBranch {
        @Function
        public RequestTreeBranch __mkReqBranch(@ReadOnly Iterable<RequestTree> trees) {
            return new RequestTreeBranch(trees);
        }
    }

    public static final class MkReqLeaf {
        @Function
        public RequestTree __mkReqTreeLeaf(@ReadOnly Request request) {
            return new RequestTreeLeaf(request);
        }
    }

    public static final class MkReqMultiLeaf {
        @Function
        public RequestTreeMultiLeaf __mkReqMultiLeaf(@ReadOnly Iterable<Request> requests) {
            return new RequestTreeMultiLeaf(requests);
        }
    }

    public static final class PackageArgs {
        @Function
        public Object[] __packageArgs(@ReadOnly Object... args) {
            return args;
        }
    }

}
