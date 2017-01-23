package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;

import java.math.BigDecimal;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {Long.class, Long.class})
public final class Modulo extends AFn {

  private static final Remainder REM = new Remainder();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "modulo";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    return apply((Number)arg1, (Number)arg2);
  }

  private BigDecimal apply(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    BigDecimal remainder = first.remainder(second);
    if (remainder.compareTo(BigDecimal.ZERO) == 0) {
      return remainder;
    }
    if ((first.compareTo(BigDecimal.ZERO) > 0) == (second.compareTo(BigDecimal.ZERO) > 0)) {
      return remainder;
    }
    return second.add(remainder);
  }

  private Number apply(Number first, Number second) {
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal) first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal) first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return apply((BigDecimal) second, new BigDecimal(first.toString()));
    }

    if (second.intValue() == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }

    Number m = REM.apply2(first, second);
    if (m.intValue() == 0) {
      return m;
    }
    if ((first.longValue() > 0) == (second.longValue() > 0)) {
      return m;
    }
    if ((first instanceof Double) || (second instanceof Double)) {
      return m.doubleValue() + second.doubleValue();
    } else {
      return m.longValue() + second.longValue();
    }
  }
}
