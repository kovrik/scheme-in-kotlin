package main.environment;

public interface IEnvironment {

  Object find(Object key);

  Object put(Object key, Object value);

}
