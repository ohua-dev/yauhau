/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import com.ohua.lang.compile.analysis.qual.ReadOnly;
import yauhau.IDataSource;
import yauhau.Request;
import yauhau.util.SimpleSelect;
import yauhau.util.Triple;
import yauhau.util.Tuple;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Created by justusadam on 12/02/16.
 */
public class AccumOp {

  public static int IO_ROUND_COUNTER = 0;
    public static int IO_FETCH_COUNTER = 0;

  public static class Accumulation {

    @Function
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
      List<Tuple<Object, Set<Triple<Integer, Request, Integer>>>> result = batchedRequests.entrySet().stream().map(
              l -> new Tuple<> (l.getKey(), l.getValue())
      ).collect(Collectors.toList());
      return result;
    }
  }

  public static class AccumulateAndFetch extends Accumulation {
    @Function
    public Object[] __accumFetch(@ReadOnly Request... requests) {
      Map<Object, Set<Request>> mappedRequests = super.__accum(requests).stream().collect(
              Collectors.toMap(
                      (Tuple<Object, Set<Triple<Integer, Request, Integer>>> t) ->
                              t.get1(),
                      (Tuple<Object, Set<Triple<Integer, Request, Integer>>> t) ->
                              t.get2().stream().map(l -> l.get2()).collect(Collectors.toSet()))
      );


      Map<Object, IDataSource> mappedSources = Arrays.stream(requests)
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
      Object[] result = Arrays.stream(requests).map(finishedRequests::get).toArray();
      IO_ROUND_COUNTER++;
      return result;
    }
  }
}
