package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Multiplication extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "*";
  }

  @Override
  public Object invoke(Object... args) {
    Object result = 1L;
    if (args != null) {
      for (Object obj : args) {
        if (!(obj instanceof Number)) {
          throw new WrongTypeException("Number", obj);
        }
        result = invoke((Number)result, (Number)obj);
      }
    }
    return result;
  }

  public static Number invoke(Number first, Number second) {
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).multiply((SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      if (second instanceof Long) {
        return ((SCMBigRational) first).multiply(new SCMBigRational(new BigInteger(second.toString()), BigInteger.ONE));
      } else {
        first = ((SCMBigRational)first).doubleOrBigDecimalValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long) {
        return ((SCMBigRational) second).multiply(new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE));
      } else {
        second = ((SCMBigRational)second).doubleOrBigDecimalValue();
      }
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.multiplyExact((Long) first, (Long) second);
      } catch (ArithmeticException e) {
        return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).multiply(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).multiply(new BigDecimal(first.toString()));
    }
    double result = first.doubleValue() * second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
    }
    return result;
  }
}
