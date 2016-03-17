/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Created by justusadam on 26/02/16.
 */
public final class RequestTreeLeaf extends RequestTree {
    private final Request request;

    public RequestTreeLeaf(Request request) {
        this.request = request;
    }

    @Override
    public Iterable<Request> getRequests() {
        List<Request> l = new ArrayList<>(1);
        l.add(request);
        return l;
    }

    @Override
    public Stream<Request> getRequestsStream() {
        return StreamSupport.stream(getRequests().spliterator(), false);
    }

    @Override
    public Iterable<Object> buildResult(Map<Request, Object> responses) {
        List<Object> l = new ArrayList<>(1);
        l.add(responses.get(request));
        return l;
    }
}
