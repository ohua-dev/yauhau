package yauhau.functions;

import com.ohua.lang.defsfn;

public final class CountImpl {
    @defsfn
    public int count(Iterable i) {
        int counter = 0;
        for (Object e : i)
            counter++;
        return counter;
    }
}
