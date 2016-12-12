package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;

import java.math.BigDecimal;

@FnArgs(args = {Long.class, Long.class})
public class Modulo extends AFn {

  // TODO move out
  private static final Remainder rem = new Remainder();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "modulo";
  }

  @Override
  public Number invoke(Object... args) {
    return invoke((Number)args[0], (Number)args[1]);
  }

  public BigDecimal invoke(BigDecimal first, BigDecimal second) {
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

  public Number invoke(Number first, Number second) {

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return invoke((BigDecimal) first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return invoke((BigDecimal) first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return invoke((BigDecimal) second, new BigDecimal(first.toString()));
    }

    // check if they are integral
    if (first.doubleValue() != Math.floor(first.doubleValue())) {
      throw new IllegalArgumentException(String.format("Error: (%s) bad argument type - not an integer: %s", getName(), first));
    }
    if (second.doubleValue() != Math.floor(second.doubleValue())) {
      throw new IllegalArgumentException(String.format("Error: (%s) bad argument type - not an integer: %s", getName(), second));
    }
    if (second.intValue() == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }

    Number m = rem.invoke(first, second);
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
