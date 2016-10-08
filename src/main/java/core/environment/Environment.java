package core.environment;

import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.*;

public class Environment implements IEnvironment, ISCMClass {

  private Map<Object, Object> context = new HashMap<Object, Object>();

  private IEnvironment outer = null;

  public Environment(IEnvironment outer) {
    this.outer = outer;
  }

  @Override
  public Object get(Object key) {
    return context.get(key);
  }

  @Override
  public boolean containsKey(Object key) {
    return context.containsKey(key);
  }

  @Override
  public List<String> getLibraryProcedures() {
    /* No pre-defined procedures. Override if required */
    return Collections.emptyList();
  }

  @Override
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

  @Override
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

  @Override
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

  @Override
  public Object put(Object key, Object value) {
    return context.put(key, value);
  }

  @Override
  public Set<Map.Entry<Object, Object>> entrySet() {
    return context.entrySet();
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ENVIRONMENT;
  }

  @Override
  public IEnvironment getOuter() {
    return outer;
  }

  @Override
  public String toString() {
    return  (outer == null) ? "GLOBAL" : "" + super.toString();
  }
}
