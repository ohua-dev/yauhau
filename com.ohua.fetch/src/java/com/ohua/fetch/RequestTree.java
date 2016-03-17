package com.ohua.fetch;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Created by justusadam on 26/02/16.
 */
public abstract class RequestTree {
    public abstract Stream<Request> getRequestsStream();

    public abstract Iterable<Object> buildResult(Map<Request, Object> responses);

    public Iterable<Request> getRequests() {
        return getRequestsStream().collect(Collectors.toList());
    }
}
