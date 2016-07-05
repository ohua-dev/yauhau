package yauhau.functions;

import com.ohua.lang.defsfn;
import java.util.Arrays;
import java.util.ArrayList;

public class Vector {

    @defsfn
    public Iterable mvector (Object... args) {
        if (null == args)
            return new ArrayList();
        return Arrays.asList(args);
    }

}
