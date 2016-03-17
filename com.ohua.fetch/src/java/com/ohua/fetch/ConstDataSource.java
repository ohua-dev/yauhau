package com.ohua.fetch;

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
