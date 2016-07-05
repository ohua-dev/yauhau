package yauhau.functions;

import com.ohua.lang.defsfn;
import java.util.Arrays;

public class Vector {

    public Vector () {
        System.out.println("Allocating vector");
    }

    @defsfn
    public Iterable mvector (Object... args) {
        System.out.println("Building vector");
        return Arrays.asList(args);
    }

}
