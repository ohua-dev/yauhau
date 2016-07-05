/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import com.ohua.lang.defsfn;
import com.ohua.lang.compile.analysis.qual.ReadOnly;
import yauhau.IDataSource;
import yauhau.Request;

import java.util.*;

/**
 * Created by sertel on 3/3/16.
 */
public class Functionality {

//  (deftype GenericPayload [#^"[Ljava.lang.Object;" args] IGenericPayload
//          (equals [this other-payload]
//                  ; my generic request payload uses Java's equals method for comparison
//                  (Arrays/deepEquals (.args this) (.args other-payload))))
//

    public static final DummyDataSource _mySource = new DummyDataSource();
    public static final SlowDummySource _mySlowSource = new SlowDummySource();

    //          (def my-kv-map {"foo" "bar"})
//          (def test-ds (reify IDataSource
//                        (getIdentifier [this] "my-kv-map")
//  (fetch [this requests] (map
//          (fn [[key time-out & deps]]
//                  (Thread/sleep time-out)
//  (get my-kv-map key))
//  requests))
//
//          (store [this requests] ((map
//          (fn [[key value time-out & deps]]
//                  (Thread/sleep time-out)
//  (let [old-val (get my-kv-map key)]
//          (set! my-kv-map (assoc my-kv-map key value))
//  old-val))
//  requests)))))
    public static int IO_FETCH_COUNTER = 0;
    public static int IO_STORE_COUNTER = 0;

    //  ;(def-sfn
//  ;  req-generic ^Request [#^"[Ljava.lang.Object;" args]
//  ;  (.Request (GenericPayload. args) test-ds))
//  ;
//  ;(def-sfn
//  ;  compute ^Object [#^"[Ljava.lang.Object;" deps ^long time-out]
//  ;  (Thread/sleep time-out)
//  ;  "some-dummy-string")
    public static int READ_REQUEST_COUNTER = 0;
    public static int WRITE_REQUEST_COUNTER = 0;

    public static class GenericPayload {
        private long _timeOut = 0;
        private String _key = null;
        private String _value = null;

        public GenericPayload(long timeOut, String key) {
            _timeOut = timeOut;
            _key = key;
        }

        public GenericPayload(long timeOut, String key, String value) {
            _timeOut = timeOut;
            _key = key;
            _value = value;
        }

        @SuppressWarnings("unchecked")
        public static Object testEquality() {
            Map hm = new HashMap<>();
            Request r1 = new Request(new GenericPayload(0, "h"), _mySource);
            Request r2 = new Request(new GenericPayload(0, "h"), _mySource);
            hm.put(r1, "k");
            assert r1.equals(r2);
            System.out.println(r1.hashCode());
            System.out.println(r2.hashCode());
            return hm.get(r2);
        }

        public String get_key() {
            return _key;
        }

        public long get_timeOut() {
            return _timeOut;
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof GenericPayload && ((GenericPayload) other).get_key().equals(_key) && ((GenericPayload) other).get_timeOut() == get_timeOut();
        }

        @Override
        public int hashCode() {
            return Objects.hash(_timeOut, _key, _value);
        }

        @Override
        public String toString() {
            return "Payload <" + _key + "> and <" + _timeOut + ">";
        }
    }

    public static class MapDataSource implements IDataSource<GenericPayload, Object> {

        private Map _kvStore = new HashMap();

        @SuppressWarnings("unchecked")
        public MapDataSource() {
            _kvStore.put("foo", "bar");
        }

        @Override
        public Object getIdentifier() {
            return "my-kv-map";
        }

        @Override
        public Iterable<Object> fetch(Iterable<GenericPayload> requests) {
            List<Object> result = new ArrayList<>();
            for (GenericPayload request : requests) {
                try {
                    Thread.sleep(request._timeOut);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                result.add(_kvStore.get(request._key));
            }
            return result;
        }

        @Override
        @SuppressWarnings("unchecked")
        public Iterable<Object> store(Iterable<GenericPayload> requests) {
            List<Object> result = new ArrayList<>();
            for (GenericPayload request : requests) {
                try {
                    Thread.sleep(request._timeOut);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                result.add(_kvStore.put(request._key, request._value));
            }
            return result;
        }
    }

    public static class DummyDataSource implements IDataSource<GenericPayload, Object> {

        public static final int DELAY_FACTOR = 100;
        public static int delay = 5;

        @Override
        public Object getIdentifier() {
            return "my-kv-map";
        }

        public int getDelay() {
            return delay;
        }

        @Override
        public Iterable<Object> fetch(Iterable<GenericPayload> requests) {
            List<Object> result = new ArrayList<>();
            for (GenericPayload request : requests) {
//        try {
//          Thread.sleep(request._timeOut);
//        } catch (InterruptedException e) {
//          e.printStackTrace();
//        }
                result.add("foo");
            }
            try {
                Thread.sleep((long) getDelay() * DELAY_FACTOR);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            IO_FETCH_COUNTER++;
            return result;
        }

        @Override
        public Iterable<Object> store(Iterable<GenericPayload> requests) {
            List<Object> result = new ArrayList<>();
            for (GenericPayload request : requests) {
//        try {
//          Thread.sleep(request._timeOut);
//        } catch (InterruptedException e) {
//          e.printStackTrace();
//        }
                result.add("old-foo");
            }
            IO_STORE_COUNTER++;
            return result;
        }
    }

    public static class SlowDummySource extends DummyDataSource {
        private static final String IDENTIFIER = "my-slow-data-source";
        public static int delay = 20;

        @Override
        public Object getIdentifier() {
            return IDENTIFIER;
        }

        @Override
        public int getDelay() {
            return delay;
        }
    }

    public static class ReadRequest {
        @defsfn
        @SuppressWarnings("unchecked")
        public Request readRequest(@ReadOnly Object... deps) {
            System.out.println("Executing read request");
            READ_REQUEST_COUNTER++;
            return new Request(new GenericPayload((long) deps[deps.length - 1], (String) deps[deps.length - 2]), _mySource);
        }
    }

    public static class SlowReadRequest {
        @defsfn
        @SuppressWarnings("unchecked")
        public Request slowReadRequest(@ReadOnly Object... deps) {
            READ_REQUEST_COUNTER++;
            return new Request(new GenericPayload((long) deps[deps.length - 1], null), _mySlowSource);
        }

    }

    public static class WriteRequest {
        @defsfn
        @SuppressWarnings("unchecked")
        public Request writeRequest(@ReadOnly Object... deps) {
            WRITE_REQUEST_COUNTER++;
            return new Request(new GenericPayload((long) deps[deps.length - 1], (String) deps[deps.length - 2], null), _mySource);
        }
    }

    public static class Computation {
        @defsfn
        public String compute(@ReadOnly Object... deps) throws InterruptedException {
//      Thread.sleep(timeOut);
            return "foo";
        }
    }

    public static class VariableDestructuring {
        @defsfn
        public Object[] vector(Object... deps) throws InterruptedException {
            return deps;
        }
    }
}
