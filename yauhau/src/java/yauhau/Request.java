/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;


import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * Represents a request that needs to be executed.
 * <p>
 * If two requests are considered 'equal' (see {@link #equals(Object)}) only one of those will be performed
 * implementing classes may want to overwrite {@link #equals(Object)} to control this behaviour.
 * <p>
 * Created by justusadam on 11/02/16.
 */
public final class Request<P, R> extends RequestTree {
    private final P payload;
    private final IDataSource<P, R> dataSource;

    public Request(P payload, IDataSource<P, R> dataSource) {
        this.payload = payload;
        this.dataSource = dataSource;
    }

    @Override
    public Stream<Request> getRequestsStream() {
        return Collections.singletonList((Request) this).stream();
    }

    @Override
    public Object buildResult(Map<Request, Object> responses) {
        return responses.get(this);
    }

    public P getPayload() {
        return payload;
    }

    public IDataSource<P, R> getDataSource() {
        return dataSource;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Request) {
            Request asR = (Request) obj;
            return getDataSource().equals(asR.getDataSource()) && asR.getPayload().equals(getPayload());
        } else
            return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(payload, dataSource);
    }

    @Override
    public String toString() {
        return payload.toString();
//        return "Request with <" + getPayload().toString() + "> on <" + getDataSource().toString() + ">";
    }

    @Override
    public int height() {
        return 1;
    }
}
