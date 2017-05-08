package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Modulo extends AFn {

  private static final Remainder REM = new Remainder();

  public Modulo() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Long.class, Long.class}).build());
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
    if (Utils.isZero(arg2)) {
      throw new ArithmeticException("modulo: undefined for 0");
    }
    return apply((Number)arg1, (Number)arg2);
  }

  private BigDecimal apply(BigDecimal first, BigDecimal second) {
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
    if (Utils.isZero(first)) {
      return Utils.inexactnessTaint(first, second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal) first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal) first, Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return apply(Utils.toBigDecimal(first), (BigDecimal) second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return apply((BigInteger) first, (BigInteger)second);
    }
    if (first instanceof BigInteger) {
      return apply((BigInteger) first, Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return apply(Utils.toBigInteger(first), (BigInteger) second);
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
