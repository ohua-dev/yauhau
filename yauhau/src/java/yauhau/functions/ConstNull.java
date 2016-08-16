/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import com.ohua.lang.defsfn;
import yauhau.Request;

/**
 * Created by justusadam on 09/03/16.
 */
public final class ConstNull {
    @defsfn
    public Object __constNull() {
        return null;
    }
}
