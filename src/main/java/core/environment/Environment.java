package core.environment;

import core.exceptions.UndefinedIdentifierException;
import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.*;

public class Environment extends HashMap<Object, Object> implements ISCMClass {

  private final Environment outer;

  public Environment(int size, Environment outer) {
    super(size);
    this.outer = outer;
  }

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
        throw new UndefinedIdentifierException(key.toString());
      }
      return outer.find(key);
    }
    return value;
  }

  public Object findOrDefault(Object key, Object defaultValue) {
    Object value = get(key);
    if (value == null) {
      if (outer == null) {
        return defaultValue;
      }
      return outer.findOrDefault(key, defaultValue);
    }
    return value;
  }

  public Object findAndPut(Object key, Object value) {
    Object v = get(key);
    if (v == null) {
      if (outer == null) {
        throw new UndefinedIdentifierException(key.toString());
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
