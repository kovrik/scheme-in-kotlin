package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRational;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class ToInexact extends AFn {

  public ToInexact() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "exact->inexact";
  }

  @Override
  public Number apply1(Object arg) {
    return toInexact(arg);
  }

  public static Number toInexact(Object o) {
    if (o instanceof BigComplex) {
      BigComplex c = ((BigComplex)o);
      return new BigComplex(toInexact(c.getRe()), toInexact(c.getIm()));
    }
    if (o instanceof BigRational) {
      return ((BigRational)o).toBigDecimalInexact();
    }
    if (o instanceof BigInteger) {
      return new BigDecimal(o.toString()).setScale(1, Utils.ROUNDING_MODE);
    }
    if (o instanceof BigDecimal) {
      int scale = Math.max(1, ((BigDecimal)o).scale());
      return ((BigDecimal)o).setScale(scale, Utils.ROUNDING_MODE);
    }
    return ((Number)o).doubleValue();
  }
}
