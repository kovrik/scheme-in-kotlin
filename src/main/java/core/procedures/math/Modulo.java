package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public final class Modulo extends AFn {

  private static final Remainder REM = new Remainder();

  public Modulo() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{Long.class, Long.class}));
  }

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
    if (second.signum() == 0) {
      throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
    }
    BigDecimal remainder = first.remainder(second);
    if (remainder.signum() == 0) {
      return remainder;
    }
    if ((first.signum() > 0) == (second.signum() > 0)) {
      return remainder;
    }
    return second.add(remainder);
  }

  private Number apply(Number first, Number second) {
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal) first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal) first, NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return apply((BigDecimal) second, NumberUtils.toBigDecimal(first));
    }

    if (second.intValue() == 0) {
      throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
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
