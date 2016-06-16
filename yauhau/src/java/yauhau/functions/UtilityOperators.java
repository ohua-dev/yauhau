/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import com.ohua.lang.defsfn;
import com.ohua.lang.compile.analysis.qual.ReadOnly;
import yauhau.*;

/**
 * Created by justusadam on 17/03/16.
 */
public class UtilityOperators {

    public static final class Identity {
        @defsfn
        public Object identity(@ReadOnly Object input) {
            return input;
        }
    }

    public static final class MkReqBranch {
        @defsfn
        public RequestTreeBranch __mkReqBranch(@ReadOnly Iterable<RequestTree> trees) {
            return new RequestTreeBranch(trees);
        }
    }

    public static final class MkReqLeaf {
        @defsfn
        public RequestTree __mkReqTreeLeaf(@ReadOnly Request request) {
            return new RequestTreeLeaf(request);
        }
    }

    public static final class MkReqMultiLeaf {
        @defsfn
        public RequestTreeMultiLeaf __mkReqMultiLeaf(@ReadOnly Iterable<Request> requests) {
            return new RequestTreeMultiLeaf(requests);
        }
    }

    public static final class PackageArgs {
        @defsfn
        public Object[] __packageArgs(@ReadOnly Object... args) {
            return args;
        }
    }

}
