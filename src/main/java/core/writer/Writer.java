package core.writer;

import core.procedures.AFn;
import core.procedures.continuations.Continuation;
import core.reader.Reader;
import core.scm.*;
import core.utils.NumberUtils;

import java.util.List;
import java.util.Map;

public class Writer implements IWriter {

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
      }
      for (Map.Entry<String, Number> entry : NumberUtils.SPECIAL_NUMBERS.entrySet()) {
        if (entry.getValue().equals(o)) {
          return entry.getKey();
        }
      }
    }
    if ((o instanceof String) || (o instanceof SCMMutableString)) {
      return "\"" + o + "\"";
    }
    if (o instanceof Character) {
      /* Check named characters */
      String named = Reader.charToNamedChar((Character) o);
      if (named != null) {
        return "#\\" + named;
      }
      return "#\\" + o;
    }
    if (o instanceof Continuation) {
      return "#<continutation>";
    }
    if (o instanceof AFn) {
      String name = ((AFn)o).getName();
      if (name == null || name.isEmpty()) {
        return "#<procedure>";
      }
      return "#<procedure:" + name + ">";
    }
    if (o instanceof Exception) {
      return ((Exception) o).getMessage();
    }
    return o.toString();
  }

  private static String writeClass(Class clazz) {
    if (ISCMClass.class.isAssignableFrom(clazz)) {
      return SCMClass.valueOf(clazz).getName();
    } else {
      if (clazz.equals(Long.class)) {
        return "Integer";
      }
    }
    return clazz.getSimpleName();
  }
}
