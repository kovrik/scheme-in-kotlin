package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {Number.class}, restArgsType = {SCMClass.ExactPositiveInteger.class})
public final class NumberToString extends AFn {

  @Override
  public String getName() {
    return "number->string";
  }

  @Override
  public String apply(Object... args) {
    Number o = (Number)args[0];
    Object o1 = null;
    if (args.length == 2) {
      o1 = args[1];
      if (!(o1.equals(2L) || o1.equals(8L) || o1.equals(10L) || o1.equals(16L))) {
        throw new IllegalArgumentException(getName() + ": bad radix (must be one of: 2, 8, 10 or 16): " + o1);
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