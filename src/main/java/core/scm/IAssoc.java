package core.scm;

public interface IAssoc {

  boolean containsKey(Object key);

  MapEntry getEntry(Object key);

  Object assoc(Object key, Object value);
}
