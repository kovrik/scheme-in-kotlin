package core.environment;

import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.*;

public class Environment implements ISCMClass {

  private Map<Object, Object> context = new HashMap<>();

  private Environment outer = null;

  public Environment(Environment outer) {
    this.outer = outer;
  }

  public Object get(Object key) {
    return context.get(key);
  }

  public boolean containsKey(Object key) {
    return context.containsKey(key);
  }

  public List<String> getLibraryProcedures() {
    /* No pre-defined procedures. Override if required */
    return Collections.emptyList();
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

  public Object findOrNull(Object key) {
    Object value = context.get(key);
    if (value == null) {
      if (outer == null) {
        return null;
      }
      return outer.findOrNull(key);
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

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ENVIRONMENT;
  }

  public Environment getOuter() {
    return outer;
  }

  @Override
  public String toString() {
    return  (outer == null) ? "GLOBAL" : "" + super.toString();
  }
}
