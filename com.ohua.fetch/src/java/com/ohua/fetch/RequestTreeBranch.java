package com.ohua.fetch;

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
    public Iterable<Object> buildResult(Map<Request, Object> responses) {
        return StreamSupport.stream(subtrees.spliterator(), false).map(t -> t.buildResult(responses)).collect(Collectors.toList());
    }
}
