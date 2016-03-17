/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;

/**
 * Created by justusadam on 29/02/16.
 */
public class PackageArgs {
    @Function
    public Object[] __packageArgs(Object... args) {
        return args;
    }
}
