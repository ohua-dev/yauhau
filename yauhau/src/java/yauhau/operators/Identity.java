/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;

/**
 * Created by justusadam on 02/03/16.
 */
public class Identity {
    @Function
    public Object identity(Object input) {
        return input;
    }
}
