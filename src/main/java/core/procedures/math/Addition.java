package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Addition extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "+";
  }

  public Number invoke(Object first, Object second) {
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).plus((SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      if (second instanceof Long || second instanceof BigDecimal) {
        return ((SCMBigRational) first).plus(new SCMBigRational(new BigInteger(second.toString()), BigInteger.ONE));
      } else {
        first = ((SCMBigRational)first).doubleOrBigDecimalValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long || first instanceof BigDecimal) {
        return ((SCMBigRational) second).plus(new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE));
      } else {
        second = ((SCMBigRational)second).doubleOrBigDecimalValue();
      }
    }

    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.addExact((Long)first, (Long)second);
      } catch (ArithmeticException e) {
        return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).add(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).add(new BigDecimal(first.toString()));
    }
    double result = ((Number)first).doubleValue() + ((Number)second).doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
    }
    return result;
  }

  @Override
  public Object invoke(Object... args) {
    Object result = 0L;
    if (args != null) {
      for (Object obj : args) {
        if (!(obj instanceof Number)) {
          throw new WrongTypeException("Number", obj);
        }
        result = invoke(result, obj);
      }
    }
    return result;
  }
}
