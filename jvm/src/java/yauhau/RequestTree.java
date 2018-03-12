/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Created by justusadam on 26/02/16.
 */
public abstract class RequestTree {
    public abstract Stream<Request> getRequestsStream();

    public abstract Object buildResult(Map<Request, Object> responses);

    public Iterable<Request> getRequests() {
        return getRequestsStream().collect(Collectors.toList());
    }

    public abstract int height();
}
