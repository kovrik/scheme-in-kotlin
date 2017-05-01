package core.writer;

import core.exceptions.ExInfoException;
import core.reader.Reader;
import core.scm.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Writer implements IWriter {

  private static final Map<Character, String> CODEPOINTS = new HashMap<>();
  static {
    Reader.NAMED_CHARS.forEach((key, value) -> CODEPOINTS.put(value, key));
  }

  @Override
  public String toString(Object o) {
    return write(o);
  }

  public static String write(Object o) {
    if (o == null) {
      return "nil";
    }
    if (o instanceof Boolean) {
      return (Boolean) o ? "#t" : "#f";
    }
    if (o instanceof SCMSymbol) {
      return ((SCMSymbol) o).isEscape() ? '|' + o.toString() + '|' : o.toString();
    }
    if (o instanceof SCMClass) {
      return "#<class:" + ((SCMClass)o).getName() + ">";
    }
    if (o instanceof Class) {
      return "#<class:" + ((Class)o).getName() + ">";
    }
    if (o instanceof List) {
      return SCMCons.toString((List) o);
    }
    if (o instanceof Double) {
      if (Double.isNaN(((Number) o).doubleValue())) {
        return "+nan.0";
      } else if (o.equals(Double.POSITIVE_INFINITY)) {
        return "+inf.0";
      } else if (o.equals(Double.NEGATIVE_INFINITY)) {
        return "-inf.0";
      }
      return o.toString();
    }
    if (o instanceof Number) {
      return o.toString();
    }
    if (o instanceof CharSequence) {
      return "\"" + o + "\"";
    }
    if (o instanceof Character) {
      /* Check named characters */
      String codepoint = CODEPOINTS.get(o);
      return codepoint == null ? "#\\" + o.toString() : "#\\" + codepoint;
    }
    if (o instanceof Exception) {
      if (o instanceof ExInfoException) {
        return o.toString();
      }
      Exception e = (Exception)o;
      return e.getMessage() == null ? e.getClass().getSimpleName() : ((Exception) o).getMessage();
    }
    if (o instanceof Map) {
      return writeMap((Map)o);
    }
    if (o instanceof Set) {
      return writeSet((Set)o);
    }
    return o.toString();
  }

  public static String writeClass(Class clazz) {
    SCMClass scmClass = SCMClass.valueOf(clazz);
    return scmClass != null ? scmClass.getName() : clazz.getSimpleName();
  }

  private static String writeMap(Map<Object, Object> map) {
    if (map.isEmpty()) {
      return  "{}";
    }
    StringBuilder sb = new StringBuilder().append('{');
    boolean first = true;
    for (Map.Entry<Object, Object> entry : map.entrySet()) {
      if (first) {
        first = false;
      } else {
        sb.append(", ");
      }
      Object key = entry.getKey();
      sb.append(key == map ? "(this hashmap)" : write(key));
      sb.append(' ');
      Object value = entry.getValue();
      sb.append(value == map? "(this hashmap)" : write(value));
    }
    return sb.append('}').toString();
  }

  private static String writeSet(Set<Object> set) {
    if (set.isEmpty()) {
      return  "#{}";
    }
    StringBuilder sb = new StringBuilder().append("#{");
    boolean first = true;
    for (Object e : set) {
      if (first) {
        first = false;
      } else {
        sb.append(' ');
      }
      sb.append(e == set ? "(this set)" : write(e));
    }
    return sb.append('}').toString();
  }
}
