package yauhau.functions;


import com.ohua.lang.defsfn;
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import java.util.Optional;


public final class Assert {
    private static final IFn APPLY = Clojure.var("clojure.core", "apply");

    public static final class AssertOp {
        @defsfn
        public Object assertOp(String marker, IFn predicate, Object arg){
            if (! (boolean) predicate.invoke(arg)) throw new RuntimeException(marker + " value: " + arg);
            return arg;
        }
    }

    public static final class AssertCallCount {
        private Optional<Integer> maxCount = Optional.empty();
        private int count = 0;
        @defsfn
        public Object assertCallCount(String message, Integer callCount, Object val) {
            if (!maxCount.isPresent())
                maxCount = Optional.of(callCount);
            count++;
            if (count > maxCount.get()) throw new RuntimeException("Call count exceeded. " + message);
            return val;
        }
    }
}
