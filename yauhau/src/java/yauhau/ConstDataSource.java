/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau;

/**
 * Created by justusadam on 01/03/16.
 */
public class ConstDataSource<A> implements IDataSource<A, A> {
    @Override
    public Object getIdentifier() {
        return IsConst.IS_CONST;
    }

    @Override
    public Iterable<A> fetch(Iterable<A> requests) {
        return requests;
    }

    @Override
    public Iterable<A> store(Iterable<A> requests) {
        return requests;
    }

    private enum IsConst {IS_CONST}
}
