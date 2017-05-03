package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Modulo extends AFn {

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

  private BigInteger apply(BigInteger first, BigInteger second) {
    if (second.signum() == 0) {
      throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
    }
    BigInteger remainder = first.remainder(second);
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
      return apply(NumberUtils.toBigDecimal(first), (BigDecimal) second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return apply((BigInteger) first, (BigInteger)second);
    }
    if (first instanceof BigInteger) {
      return apply((BigInteger) first, new BigInteger(second.toString()));
    }
    if (second instanceof BigInteger) {
      return apply(new BigInteger(first.toString()), (BigInteger) second);
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
    if ((first instanceof Double) || (second instanceof Double) || (first instanceof Float) || (second instanceof Float)) {
      return m.doubleValue() + second.doubleValue();
    }
    return m.longValue() + second.longValue();
  }
}
