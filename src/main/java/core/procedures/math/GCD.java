package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

@FnArgs(restArgsType = SCMBigRational.class)
public final class GCD extends AFn {

  private static final Abs ABS = new Abs();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "gcd";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 0) {
      return 0L;
    }
    if (args.length == 1) {
      return ABS.apply1(args[0]);
    }
    return (Number) Arrays.stream(args).reduce((f, s) -> gcd((Number)f, (Number)s)).get();
  }

  static long gcd(Long a, Long b) {
    while (b > 0) {
      long temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  }

  static Number gcd(Double a, Double b) {
    if (a.isInfinite() || a.isNaN()) {
      throw new WrongTypeException("Integer", a);
    }
    if (b.isInfinite() || b.isNaN()) {
      throw new WrongTypeException("Integer", b);
    }
    if (a.longValue() != a || b.longValue() != b) {
      return ToInexact.toInexact(gcd(ToExact.toExact(a), ToExact.toExact(b)));
    }
    return (double)gcd(a.longValue(), b.longValue());
  }

  static Number gcd(BigDecimal a, BigDecimal b) {
    int scale = Math.max(a.scale(), b.scale());
    if (scale == 0) {
      return new BigDecimal(a.toBigInteger().gcd(b.toBigInteger()));
    } else {
      return ToInexact.toInexact(gcd(ToExact.toExact(a), ToExact.toExact(b)));
    }
  }

  static BigInteger gcd(BigInteger a, BigInteger b) {
    return a.gcd(b);
  }

  static SCMBigRational gcd(SCMBigRational first, SCMBigRational second) {
    return new SCMBigRational(first.getNumerator().gcd(second.getNumerator()),
                              LCM.lcm(first.getDenominator(), second.getDenominator()));
  }

  private static Number gcd(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return gcd((Long)first, (Long)second);
    }
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return gcd((SCMBigRational) first, (SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      return gcd(((SCMBigRational) first).toBigDecimal(), new BigDecimal(second.toString()));
    }
    if (second instanceof SCMBigRational) {
      return gcd(new BigDecimal(first.toString()), ((SCMBigRational) second).toBigDecimal());
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return gcd((BigDecimal) first, (BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return gcd((BigDecimal) first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return gcd(new BigDecimal(first.toString()), (BigDecimal) second);
    }
    return gcd(first.doubleValue(), second.doubleValue());
  }
}
