package core.writer;

import core.procedures.IFn;
import core.reader.Reader;
import core.scm.ISCMClass;
import core.scm.ISCMPort;
import core.scm.SCMBigRational;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMInputPort;
import core.scm.SCMOutputPort;
import core.scm.SCMPromise;
import core.scm.SCMString;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
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
    if (o instanceof Class) {
      return writeClass((Class) o);
    }
    if (o instanceof List) {
      return SCMCons.toString((List) o);
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
    if ((o instanceof String) || (o instanceof SCMString)) {
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

  private static String writeClass(Class clazz) {
    if (ISCMClass.class.isAssignableFrom(clazz)) {
      if (clazz.equals(SCMString.class)) {
        return "String";
      } else if (clazz.equals(SCMBoolean.class)) {
        return "Boolean";
      } else if (clazz.equals(SCMSymbol.class)) {
        return "Symbol";
      } else if (IFn.class.isAssignableFrom(clazz)) {
        return "Procedure";
      } else if (clazz.equals(SCMBigRational.class)) {
        return "Rational";
      } else if (clazz.equals(SCMVector.class)) {
        return "Vector";
      } else if (clazz.equals(SCMPromise.class)) {
        return "Promise";
      } else if (clazz.equals(ISCMPort.class)) {
        return "Port";
      } else if (clazz.equals(SCMOutputPort.class)) {
        return "Output Port";
      } else if (clazz.equals(SCMInputPort.class)) {
        return "Input Port";
      }
    } else {
      if (clazz.equals(Long.class)) {
        return "Integer";
      }
    }
    return clazz.getSimpleName();
  }
}
