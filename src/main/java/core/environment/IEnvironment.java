package core.environment;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface IEnvironment {

  /* Get key from this environment */
  Object get(Object key);

  /* Try to find key recursively and throw exception if not found */
  Object find(Object key);

  Object findAndPut(Object key, Object value);

  Object put(Object key, Object value);

  Set<Map.Entry<Object, Object>> entrySet();

  boolean containsKey(Object key);

  /* Return a list of pre-defined (standard) library procedures in form of strings (Scheme expressions) */
  List<String> getLibraryProcedures();
}
