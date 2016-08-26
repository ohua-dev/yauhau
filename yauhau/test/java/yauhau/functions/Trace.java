package yauhau.functions;


import com.ohua.lang.defsfn;

import java.util.Arrays;


public final class Trace<A> {
    @defsfn
    public A trace(Object marker, A a){
        String printable;
        if (a instanceof Object[])
            printable = Arrays.toString((Object[]) a);
        else
            printable = a.toString();
        System.out.println(marker.toString() + ": " + printable);
        return a;
    }
}
