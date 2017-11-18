/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

import java.util.*;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Created by justusadam on 26/02/16.
 */
public final class RequestTreeLeaf extends RequestTree {
    private final Request request;
    private final List<Request> reqAsList;

    public RequestTreeLeaf(Request request) {
        this.request = request;
        this.reqAsList = Arrays.asList(request);
    }

    @Override
    public Iterable<Request> getRequests() {
        return reqAsList;
    }

    @Override
    public Stream<Request> getRequestsStream() {
        return reqAsList.stream();
    }

    @Override
    public Object buildResult(Map<Request, Object> responses) {
        return Arrays.asList(responses.get(request));
    }

    @Override
    public int height() {
        return 1;
    }
}
