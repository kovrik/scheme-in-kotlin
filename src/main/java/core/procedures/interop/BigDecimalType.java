package core.procedures.interop;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.math.BigDecimal;

public final class BigDecimalType extends AFn {

  public BigDecimalType() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bigdec";
  }

  @Override
  public BigDecimal apply1(Object arg) {
    if (arg instanceof Number) {
      return Utils.toBigDecimal((Number) arg);
    }
    return new BigDecimal(arg.toString());
  }
}
