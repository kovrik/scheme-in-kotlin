package main.environment;

import java.util.HashMap;
import java.util.Map;

public class Environment {

  private Map<Object, Object> parameters = new HashMap<Object, Object>();

  private Environment outer = null;

  public Environment(Environment outer) {
    this.outer = outer;
  }

  public Environment(Map<Object, Object> values, Environment outer) {
    this.parameters.putAll(values);
    this.outer = outer;
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
}
