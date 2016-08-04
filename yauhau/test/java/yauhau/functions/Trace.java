package yauhau.functions;


import com.ohua.lang.defsfn;


public final class Trace<A> {
    @defsfn
    public A trace(Object marker, A a){
        System.out.println(marker.toString() + ": " + a.toString());
        return a;
    }
}
