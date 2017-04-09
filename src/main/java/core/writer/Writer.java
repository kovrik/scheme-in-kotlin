package core.writer;

import core.reader.Reader;
import core.scm.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Writer implements IWriter {

  private static final Map<Character, String> CODEPOINTS = new HashMap<>();
  static {
    Reader.NAMED_CHARS.entrySet().forEach(e -> CODEPOINTS.put(e.getValue(), e.getKey()));
  }

  @Override
  public String toString(Object o) {
    return write(o);
  }

  public static String write(Object o) {
    if (o == null) {
      return SCMConstant.NIL.toString();
    }
    if (o instanceof Boolean) {
      if ((Boolean) o) {
        return "#t";
      } else {
        return "#f";
      }
    }
    if (o instanceof SCMSymbol) {
      if (((SCMSymbol) o).isEscape()) {
        return '|' + o.toString() + '|';
      }
      return o.toString();
    }
    if (o instanceof Class) {
      SCMClass scmClass = SCMClass.valueOf((Class) o);
      String name = ((Class)o).getSimpleName();
      if (scmClass == null) {
        name = ((Class)o).getName();
      }
      return "#<class:" + name + ">";
    }
    if (o instanceof List) {
      return SCMCons.toString((List) o);
    }
    if (o instanceof SCMBigComplex) {
      return o.toString();
    }
    if (o instanceof Number) {
      if (Double.isNaN(((Number) o).doubleValue())) {
        return "+nan.0";
      } else if (o.equals(Double.POSITIVE_INFINITY)) {
        return "+inf.0";
      } else if (o.equals(Double.NEGATIVE_INFINITY)) {
        return "-inf.0";
      }
      return o.toString();
    }
    if ((o instanceof String) || (o instanceof SCMMutableString)) {
      return "\"" + o + "\"";
    }
    if (o instanceof Character) {
      /* Check named characters */
      return "#\\" + CODEPOINTS.getOrDefault(o, o.toString());
    }
    if (o instanceof Exception) {
      return ((Exception) o).getMessage();
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
    if (scmClass != null) {
      return scmClass.getName();
    }
    return clazz.getSimpleName();
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
