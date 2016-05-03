package main.core.environment;

public interface IEnvironment {

  Object get(Object key);

  Object find(Object key);

  Object put(Object key, Object value);

}
