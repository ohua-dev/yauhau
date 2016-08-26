package yauhau;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Created by justusadam on 24/06/16.
 */
public final class IncDataSource implements IDataSource<Integer, Integer> {
    public Integer counter = 0;

    public IncDataSource () {}

    public IncDataSource (Integer initialCounter) {
        this.counter = initialCounter;
    }

    private static Integer inc(Integer a) {
        return a + 1;
    }

    @Override
    public Object getIdentifier() {
        return "incrementer";
    }

    @Override
    public Iterable<Integer> fetch(Iterable<Integer> requests) {
        this.counter ++;
        return StreamSupport.stream(requests.spliterator(), true).map(IncDataSource::inc).collect(Collectors.toList());
    }

    @Override
    public Iterable<Integer> store(Iterable<Integer> requests) {
        return null;
    }
}
