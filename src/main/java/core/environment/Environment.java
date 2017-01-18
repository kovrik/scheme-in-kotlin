package core.environment;

import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.*;

public class Environment extends HashMap<Object, Object> implements ISCMClass {

  private final Environment outer;

  public Environment(Environment outer) {
    this.outer = outer;
  }

  public List<String> getLibraryProcedures() {
    /* No pre-defined procedures. Override if required */
    return Collections.emptyList();
  }

  public Object find(Object key) {
    Object value = get(key);
    if (value == null) {
      if (outer == null) {
        throw new IllegalArgumentException("Unbound variable: " + key);
      }
      return outer.find(key);
    }
    return value;
  }

  public Object findAndPut(Object key, Object value) {
    Object v = get(key);
    if (v == null) {
      if (outer == null) {
        throw new IllegalArgumentException("Unbound variable: " + key);
      }
      return outer.findAndPut(key, value);
    }
    return put(key, value);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ENVIRONMENT;
  }

  @Override
  public String toString() {
    return "#<environment:" + ((outer == null) ? "GLOBAL" : super.toString()) + ">";
  }
}
