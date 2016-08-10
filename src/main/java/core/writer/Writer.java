package core.writer;

import core.reader.Reader;
import core.scm.SCMCons;
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
    if (o instanceof List) {
      return SCMCons.toString((List)o);
    }
    if (o instanceof Number) {
      if (Double.isNaN(((Number)o).doubleValue())) {
        return "+nan.0";
      }
      for (Map.Entry<String, Number> entry : NumberUtils.SPECIAL_NUMBERS.entrySet()) {
        if (entry.getValue().equals(o)) {
          return entry.getKey();
        }
      }
    }
    if (o instanceof String) {
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
    if (o instanceof Exception) {
      return ((Exception) o).getMessage();
    }
    return o.toString();
  }
}
