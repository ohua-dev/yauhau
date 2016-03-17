package com.ohua.fetch.util;


import java.util.function.BinaryOperator;

/**
 * Created by justusadam on 12/02/16.
 */
@FunctionalInterface
public interface SimpleSelect<T> extends BinaryOperator<T> {
    static <T> SimpleSelect<T> first() {
        return (a, b) -> a;
    }

    static <T> SimpleSelect<T> second() {
        return (a, b) -> b;
    }
}
