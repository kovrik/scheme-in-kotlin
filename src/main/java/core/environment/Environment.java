package core.environment;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class Environment implements IEnvironment {

  private Map<Object, Object> context = new HashMap<Object, Object>();

  private IEnvironment outer = null;

  public Environment(IEnvironment outer) {
    this.outer = outer;
  }

  public Environment(Map<?, ?> params, IEnvironment outer) {
    this.context.putAll(params);
    this.outer = outer;
  }

  public Environment(IEnvironment params, IEnvironment outer) {
    if (params != null) {
      for (Map.Entry<Object, Object> entry : params.entrySet()) {
        this.context.put(entry.getKey(), entry.getValue());
      }
    }
    this.outer = outer;
  }

  public Object get(Object key) {
    return context.get(key);
  }

  public Object find(Object key) {
    Object value = context.get(key);
    if (value == null) {
      if (outer == null) {
        throw new IllegalArgumentException("Unbound variable: " + key);
      }
      return outer.find(key);
    }
    return value;
  }

  public Object findAndPut(Object key, Object value) {
    Object v = context.get(key);
    if (v == null) {
      if (outer == null) {
        throw new IllegalArgumentException("Unbound variable: " + key);
      }
      return outer.findAndPut(key, value);
    }
    return context.put(key, value);
  }

  public Object put(Object key, Object value) {
    return context.put(key, value);
  }

  public void putAll(Map<?, ?> params) {
    context.putAll(params);
  }

  public Set<Map.Entry<Object, Object>> entrySet() {
    return context.entrySet();
  }
}
