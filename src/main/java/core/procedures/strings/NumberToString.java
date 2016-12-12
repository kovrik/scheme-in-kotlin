package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(isVariadic = true, args = {Number.class})
public class NumberToString extends AFn {

  @Override
  public String getName() {
    return "number->string";
  }

  @Override
  public String invoke(Object... args) {
    Number o = (Number)args[0];
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
      if (NumberUtils.isInteger(bigDecimal)) {
        return bigDecimal.toBigInteger().toString(radix);
      }
      throw new IllegalArgumentException(getName() + ": inexact numbers can only be printed in base 10");
    }
    return o.toString();
  }
}