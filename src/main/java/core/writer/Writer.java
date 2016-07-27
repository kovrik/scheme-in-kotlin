package core.writer;

import core.reader.Reader;
import core.scm.SCMCons;

import java.util.List;

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
