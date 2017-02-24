package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

import static core.procedures.math.GCD.gcd;

public final class LCM extends AFn {

  private static final Abs ABS = new Abs();

  public LCM() {
    super(new FnArgsBuilder().restArgsType(new Class[]{SCMBigRational.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "lcm";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 0) {
      return 1L;
    }
    if (args.length == 1) {
      return ABS.apply1(args[0]);
    }
    return (Number) Arrays.stream(args).reduce((f, s) -> lcm((Number)f, (Number)s)).get();
  }

  private static long lcm(Long a, Long b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0L;
    }
    return (a / gcd(a, b)) * b;
  }

  private static Double lcm(Double a, Double b) {
    if ((a.intValue() == 0) && (b.intValue() == 0)) {
      return 0d;
    }
    return (a / gcd(a, b).doubleValue()) * b;
  }

  static BigInteger lcm(BigInteger first, BigInteger second) {
    return first.multiply(second.divide(gcd(first, second)));
  }

  private SCMBigRational lcm(SCMBigRational first, SCMBigRational second) {
    return new SCMBigRational(lcm(first.getNumerator(), second.getNumerator()),
                              gcd(first.getDenominator(), second.getDenominator()));
  }

  private Number lcm(BigDecimal a, BigDecimal b) {
    if ((BigDecimal.ZERO.compareTo(a) == 0) && (BigDecimal.ZERO.compareTo(b) == 0)) {
      return BigDecimal.ZERO;
    }
    int scale = Math.max(a.scale(), b.scale());
    if (scale == 0) {
      return new BigDecimal(lcm(a.toBigInteger(), b.toBigInteger()));
    } else {
      return ToInexact.toInexact(lcm((BigDecimal)ToExact.toExact(a), (BigDecimal) ToExact.toExact(b)));
    }
  }

  private Number lcm(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      return lcm((Long)first, (Long)second);
    }
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return lcm((SCMBigRational) first, (SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      return lcm(((SCMBigRational) first).toBigDecimal(), new BigDecimal(second.toString()));
    }
    if (second instanceof SCMBigRational) {
      return lcm(new BigDecimal(first.toString()), ((SCMBigRational) second).toBigDecimal());
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return lcm((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return lcm((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return lcm(new BigDecimal(first.toString()), (BigDecimal)second);
    }
    return lcm(first.doubleValue(), second.doubleValue());
  }
}
