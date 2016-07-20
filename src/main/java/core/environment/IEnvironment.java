package core.environment;

import java.util.Map;
import java.util.Set;

public interface IEnvironment {

  Object get(Object key);

  Object find(Object key);

  Object findAndPut(Object key, Object value);

  Object put(Object key, Object value);

  Set<Map.Entry<Object, Object>> entrySet();

  boolean containsKey(Object key);
}
