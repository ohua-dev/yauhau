package com.ohua.fetch.operators;

import com.ohua.engine.flowgraph.elements.operator.OutputMatch;
import com.ohua.fetch.IDataSource;
import com.ohua.fetch.Request;
import com.ohua.fetch.util.Triple;
import com.ohua.fetch.util.Tuple;
import com.ohua.lang.Function;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by sertel on 3/8/16.
 */
public class ConcurrentIO {

  public static class Fetch {
    @Function
    public List<Triple<Integer, Object, Integer>> __batchedFetch(Tuple<Object, Set<Triple<Integer, Request, Integer>>> batchedRequest){
      IDataSource ds = batchedRequest.get2().iterator().next().get2().getDataSource();
      Iterable resps = ds.fetch(batchedRequest.get2().stream().map(l -> l.get2().getPayload()).collect(Collectors.toList()));
      Iterator<Triple<Integer, Request, Integer>> reqIt = batchedRequest.get2().iterator();
      Iterator respsIt = resps.iterator();
      List<Triple<Integer, Object, Integer>> result = new ArrayList<>();
      while(respsIt.hasNext() && reqIt.hasNext()) {
        Triple<Integer, Request, Integer> req = reqIt.next();
        result.add(new Triple<>(req.get1(), respsIt.next(), req.get3()));
      }

      assert !respsIt.hasNext() && !reqIt.hasNext();
      return result;
    }
  }

  public static class Unbatching {
    @Function
    public Object[] __unbatch(List<Triple<Integer, Object, Integer>> batchedResponse){
      Object[] result = new Object[batchedResponse.get(0).get3()];
      Arrays.fill(result, OutputMatch.OutputMatcher.Control.DROP);
      batchedResponse.stream().forEach(l -> result[l.get1()] = l.get2());
      return result;
    }
  }
}
