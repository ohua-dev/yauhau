/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.util;

/**
 * Created by sertel on 3/8/16.
 */
public class Triple<T1, T2, T3> extends Tuple<T1, T2> {
  private final T3 _3;

  public Triple(T1 _1, T2 _2, T3 _3) {
    super(_1, _2);
    this._3 = _3;
  }

  public T3 get3() {
    return _3;
  }

}
