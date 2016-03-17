/*
 * Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE file.
 */

package yauhau.operators;

import com.ohua.lang.Function;
import com.ohua.lang.compile.analysis.qual.ReadOnly;
import yauhau.Request;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * Created by sertel on 3/7/16.
 */
public class Caching {

  public final static MapCache _cache = new MapCache(StoreHandlers.dropOne());
  private final static MapCache _flushCache = new MapCache(StoreHandlers.dropAll());

  public static Map getPersistentCacheMap() {
    return _cache.getInnerMap();
  }

  interface Dropable<A> {
    Dropable<A> dropOne(A a);

    Dropable<A> dropAll();
  }

  interface Cache {
    BiFunction<Dropable<Request>, Request, Dropable<Request>> getRemoveAction();

    Object get(Request r);

    Object set(Request r, Object resp);

    void handleStore(Request r);

    int cacheSize();
  }

  @FunctionalInterface
  public interface StoreHandlers extends BiFunction<Dropable<Request>, Request, Dropable<Request>> {
    static BiFunction<Dropable<Request>, Request, Dropable<Request>> dropOne() {
      return Dropable::dropOne;
    }

    static BiFunction<Dropable<Request>, Request, Dropable<Request>> dropAll() {
      return (cache, req) -> cache.dropAll();
    }

  }

  public static abstract class StdCache implements Cache, Dropable<Request> {
    private BiFunction<Dropable<Request>, Request, Dropable<Request>> _action;

    protected StdCache() {
    }

    public StdCache(BiFunction<Dropable<Request>, Request, Dropable<Request>> action) {
      _action = action;
    }

    @Override
    public BiFunction<Dropable<Request>, Request, Dropable<Request>> getRemoveAction() {
      return _action;
    }

    @Override
    public void handleStore(Request r) {
      _action.apply(this, r);
    }
  }

  private static class MapCache extends StdCache {

    private Map<Request, Object> _cache = new HashMap<>();

    public MapCache(BiFunction<Dropable<Request>, Request, Dropable<Request>> action) {
      super(action);
    }

    public Object get(Request r) {
      return _cache.get(r);
    }

    public Object set(Request r, Object resp) {
      return _cache.put(r, resp);
    }

    public StdCache dropOne(@ReadOnly Request r) {
      _cache.remove(r);
      return this;
    }

    public StdCache dropAll() {
      _cache.clear();
      return this;
    }

    public Map getInnerMap() {
      return _cache;
    }

    @Override
    public int cacheSize() {
      return _cache.size();
    }
  }

  public static class RoundPersistentCache {
    @Function
    public Cache roundPersistentCacheManager() {
      return new MapCache(StoreHandlers.dropOne());
    }
  }

  public static class RoundPersistentFlushCache {
    @Function
    public Cache roundPersistentFlushCacheManager() {
      return new MapCache(StoreHandlers.dropAll());
    }
  }

  public static class CachedAccumFetch extends AccumOp.AccumulateAndFetch {
    @Function
    public Object[] __cachedAccumFetch(Cache cache, @ReadOnly Request... requests) {
      // load requests that are not fetched yet
      List<Request> nonCachedRequests = Arrays.stream(requests).filter(l -> cache.get(l) == null).collect(Collectors.toList());
      Object[] fetchedResults;
      if (nonCachedRequests.size() > 0) {
        fetchedResults = super.__accumFetch(nonCachedRequests.toArray(new Request[nonCachedRequests.size()]));
      } else {
        fetchedResults = new Object[]{};
      }
      assert nonCachedRequests.size() == fetchedResults.length;

      // load the fetched requests into the cache
      for (int i = 0; i < nonCachedRequests.size(); i++)
        cache.set(nonCachedRequests.get(i), fetchedResults[i]);

      // construct the proper output again
      return Arrays.stream(requests).map(cache::get).toArray();
    }
  }

  public static class RequestPersistentCache {
    @Function
    public Cache requestPersistentCacheManager() {
      return _cache;
    }
  }

  public static class RequestPersistentFlushCache {
    @Function
    public Cache requestPersistentFlushCacheManager() {
      return _flushCache;
    }
  }

  public static class CachedStore {
    private final Store storeOp = new Store();

    @Function
    @SuppressWarnings("unchecked")
    public Object __cachedStore(Cache cache, @ReadOnly Request request) {
      cache.handleStore(request);
      return storeOp.store(request);
    }
  }

}
