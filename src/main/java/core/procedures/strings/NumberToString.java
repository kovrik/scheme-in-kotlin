package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class NumberToString extends AFn {

  @Override
  public String getName() {
    return "number->string";
  }

  @Override
  public String invoke(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, getName());
    }
    Object o = args[0];
    if (!(o instanceof Number)) {
      throw new WrongTypeException("Number", o);
    }
    if (args.length > 2) {
      throw new ArityException(args.length, getName());
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
    int radix = (o1 != null) ? ((Number)o1).intValue() : 10;
    if (o instanceof Long) {
      return Long.toString((Long)o, radix);
    }
    if (o instanceof Double) {
      if (radix != 10) {
        throw new IllegalArgumentException(getName() + ": inexact numbers can only be printed in base 10");
      }
      return o.toString();
    }
    if (o instanceof BigDecimal) {
      BigDecimal bigDecimal = (BigDecimal) o;
      if (radix == 10) {
        return bigDecimal.toString();
      }
      /* Check if it is integral */
      if (bigDecimal.remainder(BigDecimal.ONE).equals(BigDecimal.ZERO)) {
        return bigDecimal.toBigInteger().toString(radix);
      }
      throw new IllegalArgumentException(getName() + ": inexact numbers can only be printed in base 10");
    }
    return o.toString();
  }
}