/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Created by justusadam on 26/02/16.
 */
public final class RequestTreeBranch extends RequestTree {
    private final Iterable<RequestTree> subtrees;

    public RequestTreeBranch(Iterable<RequestTree> subtrees) {
        this.subtrees = subtrees;
    }

    @Override
    public Stream<Request> getRequestsStream() {
        return StreamSupport.stream(subtrees.spliterator(), false).flatMap(RequestTree::getRequestsStream);
    }

    @Override
    public Object buildResult(Map<Request, Object> responses) {
        List<Object> res = StreamSupport.stream(subtrees.spliterator(), false).map(t -> t.buildResult(responses)).collect(Collectors.toList());
//        System.out.println(res.size());
//        System.out.println(res);
        return res;
    }

    @Override
    public String toString() {
        return subtrees.toString();
    }

    @Override
    public int height() {
        return 1 + subtrees.iterator().next().height();
    }
}
