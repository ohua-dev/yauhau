/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

/**
 * Created by justusadam on 11/02/16.
 */
public interface IDataSource<P, R> {

    /**
     * Returns a unique identifier for this data source.
     * Used for routing requests.
     *
     * @return
     */
    Object getIdentifier();

    /**
     * Performs a collection of requests.
     *
     * The iterable returned must have the same length as the 'requests' iterable and contain the
     * fetch results in the same order.
     *
     * @param requests
     * @return
     */
    Iterable<R> fetch(Iterable<P> requests);

    /**
     * Performs a collection of requests.
     *
     * The iterable returned must have the same length as the 'requests' iterable and contain the
     * store results in the same order.
     *
     * @param requests
     * @return
     */
    Iterable<R> store(Iterable<P> requests);
}
