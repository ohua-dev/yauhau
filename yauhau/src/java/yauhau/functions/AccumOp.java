/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.functions;

import com.ohua.lang.compile.analysis.qual.ReadOnly;
import com.ohua.lang.defsfn;
import yauhau.IDataSource;
import yauhau.Request;
import yauhau.RequestTree;
import yauhau.util.SimpleSelect;
import yauhau.util.Triple;
import yauhau.util.Tuple;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Implementation for accumulation functions.
 */
public class AccumOp {

    public static int IO_ROUND_COUNTER = 0;
    public static int IO_FETCH_COUNTER = 0;

    public static class Accumulation {

        @defsfn
        public List<Tuple<Object, Set<Triple<Integer, Request, Integer>>>> __accum(@ReadOnly Request... requests) {
            IO_FETCH_COUNTER += requests.length;
            Map<Object, Set<Triple<Integer, Request, Integer>>> batchedRequests = IntStream.range(0, requests.length).boxed().map(
                    l -> new Triple<>(l, requests[l], requests.length)
            ).collect(
                    Collectors.toMap(
                            (Triple<Integer, Request, Integer> reqIdx) -> reqIdx.get2().getDataSource().getIdentifier(),
                            (Triple<Integer, Request, Integer> reqIdx) -> {
                                Set<Triple<Integer, Request, Integer>> l = new HashSet<>();
                                l.add(reqIdx);
                                return l;
                            },
                            (Set<Triple<Integer, Request, Integer>> l1, Set<Triple<Integer, Request, Integer>> l2) -> {
                                l1.addAll(l2);
                                return l1;
                            })
            );
            return batchedRequests.entrySet().stream().map(
                    l -> new Tuple<>(l.getKey(), l.getValue())
            ).collect(Collectors.toList());
        }
    }

    public static class AccumulateAndFetch extends Accumulation {
        @defsfn
        public Object[] __accumFetch(@ReadOnly RequestTree... requests) throws Throwable {

            for (int i=0; i < requests.length; i++)
                if (requests[i] == null) throw new Exception("Request tree " + i + " of " + requests.length + " is null.");

            Request[] rqarr = Arrays.stream(requests).flatMap(RequestTree::getRequestsStream).toArray(Request[]::new);

            for (int i=0; i < rqarr.length; i++)
                if (rqarr[i] == null) throw new Exception("Request " + i + " is null.");

            Map<Object, Set<Request>> mappedRequests = super.__accum(rqarr).stream().collect(
                    Collectors.toMap(
                            Tuple::get1,
                            (Tuple<Object, Set<Triple<Integer, Request, Integer>>> t) ->
                                    t.get2().stream().map(Tuple::get2).collect(Collectors.toSet()))
            );


            Map<Object, IDataSource> mappedSources = Arrays.stream(requests).flatMap(RequestTree::getRequestsStream)
                    .map(Request::getDataSource).collect(Collectors.toMap(
                            IDataSource::getIdentifier,
                            java.util.function.Function.identity(),
                            SimpleSelect.second()
                    ));

            Map<Request, Object> finishedRequests = mappedSources.keySet().stream().flatMap(
                    (Object identifier) -> {
                        Set<Request> currRequests = mappedRequests.get(identifier);
                        IDataSource dataSource = mappedSources.get(identifier);

                        @SuppressWarnings("unchecked")
                        Iterable fetched = dataSource.fetch(currRequests.stream().map(Request::getPayload).collect(Collectors.toList()));
                        Iterator<Request> requestIterator = currRequests.iterator();
                        Iterator resultIterator = fetched.iterator();
                        List<Tuple<Request, Object>> res = new ArrayList<>();

                        while (requestIterator.hasNext() && resultIterator.hasNext())
                            res.add(new Tuple<>(requestIterator.next(), resultIterator.next()));

                        return res.stream();
                    }
            ).collect(Collectors.toMap(
                    Tuple::get1,
                    Tuple::get2
            ));
            Object[] result = Arrays.stream(requests).map(r -> r.buildResult(finishedRequests)).toArray();
            IO_ROUND_COUNTER++;
            return result;
        }
    }
}
