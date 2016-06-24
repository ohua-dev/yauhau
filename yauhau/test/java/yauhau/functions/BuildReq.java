package yauhau.functions;

import com.ohua.lang.defsfn;
import yauhau.IDataSource;
import yauhau.Request;

/**
 * Created by justusadam on 24/06/16.
 */
public class BuildReq {
    @defsfn
    @SuppressWarnings("unchecked")
    public Request mkReq (Object a, IDataSource b) {
        return new Request(a, b);
    }
}
