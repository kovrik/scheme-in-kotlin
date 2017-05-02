package core.procedures.interop;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.math.BigDecimal;

public final class BigDecimalType extends AFn {

  public BigDecimalType() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    if (arg instanceof Long) {
      return BigDecimal.valueOf((long)arg);
    }
    if (arg instanceof Double) {
      return BigDecimal.valueOf((double)arg);
    }
    return new BigDecimal(arg.toString());
  }
}
