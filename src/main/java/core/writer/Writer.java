package core.writer;

import core.reader.Reader;
import core.scm.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
      return SCMCons.NIL.toString();
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
      return writeClass((Class) o);
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
      } else if (((Number)o).doubleValue() == Double.POSITIVE_INFINITY) {
        return "+inf.0";
      } else if (((Number)o).doubleValue() == Double.NEGATIVE_INFINITY) {
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
    return o.toString();
  }

  private static String writeClass(Class clazz) {
    SCMClass scmClass = SCMClass.valueOf(clazz);
    if (scmClass != null) {
      return scmClass.getName();
    }
    return clazz.getSimpleName();
  }
}
