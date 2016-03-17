package com.ohua.fetch;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Created by justusadam on 26/02/16.
 */
public final class RequestTreeMultiLeaf extends RequestTree {
    private final Iterable<Request> requests;

    public RequestTreeMultiLeaf(Iterable<Request> requests) {
        this.requests = requests;
    }

    @Override
    public Iterable<Request> getRequests() {
        return requests;
    }

    @Override
    public Stream<Request> getRequestsStream() {
        return StreamSupport.stream(getRequests().spliterator(), false);
    }

    @Override
    public Iterable<Object> buildResult(Map<Request, Object> responses) {
        return getRequestsStream().map(responses::get).collect(Collectors.toList());
    }
}
