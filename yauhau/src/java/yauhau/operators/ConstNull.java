/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import yauhau.Request;

/**
 * Created by justusadam on 09/03/16.
 */
public final class ConstNull {
    @Function
    public Request __constNull() {
        return null;
    }
}
