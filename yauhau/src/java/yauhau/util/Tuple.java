/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.util;

/**
 * Created by justusadam on 12/02/16.
 */
public class Tuple<T1, T2> {
    private final T1 _1;
    private final T2 _2;

    public Tuple(T1 _1, T2 _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public T1 get1() {
        return _1;
    }

    public T2 get2() {
        return _2;
    }
}
