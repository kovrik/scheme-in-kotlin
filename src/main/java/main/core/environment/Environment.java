package main.core.environment;

import java.util.HashMap;
import java.util.Map;

public class Environment implements IEnvironment {

  private Map<Object, Object> parameters = new HashMap<Object, Object>();

  private IEnvironment outer = null;

  public Environment(IEnvironment outer) {
    this.outer = outer;
  }

  public Environment(Map<?, ?> params, IEnvironment outer) {
    this.parameters.putAll(params);
    this.outer = outer;
  }

  public Object get(Object key) {
    return parameters.get(key);
  }

  public Object find(Object key) {
    Object value = parameters.get(key);
    if (value == null) {
      if (outer == null) {
        throw new IllegalArgumentException("Unbound variable: " + key);
      }
      return outer.find(key);
    }
    return value;
  }

  public Object put(Object key, Object value) {
    return parameters.put(key, value);
  }

  public void putAll(Map<?, ?> params) {
    parameters.putAll(params);
  }
}
