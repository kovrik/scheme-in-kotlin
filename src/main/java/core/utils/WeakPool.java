package core.utils;

import java.lang.ref.WeakReference;
import java.util.WeakHashMap;

public class WeakPool<T> {

  private final WeakHashMap<T, WeakReference<T>> pool = new WeakHashMap<>();

  public T get(T object) {
    final T res;
    WeakReference<T> ref = pool.get(object);
    if (ref != null) {
      res = ref.get();
    } else {
      res = null;
    }
    return res;
  }

  public void put(T object) {
    pool.put(object, new WeakReference<>(object));
  }
}
