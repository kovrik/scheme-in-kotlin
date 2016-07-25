package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringToNumber extends AFn {

  @Override
  public String invoke(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, "string->number");
    }
    Object o = args[0];
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }

    /* Read number */
    Integer radix  = null;
    // TODO

    /* Get radix */
    if (args.length > 2) {
      throw new ArityException(args.length, "string->number");
    }
    Object o1 = null;
    if (args.length == 2) {
      o1 = args[1];
      if (!(o1 instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      if (!(o1.equals(2L) || o1.equals(8L) || o1.equals(10L) || o1.equals(16L))) {
        throw new IllegalArgumentException("Wrong radix: " + o1);
      }
    }

    // TODO
    /* Convert */
    if (radix == null) {
      if (o1 != null) {
        radix = (Integer)o1;
      } else {
        radix = 10;
      }
    }

    return o.toString();
  }
}