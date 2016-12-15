package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.math.*;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Magnitude extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "magnitude";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof SCMBigComplex) {
      SCMBigComplex c = (SCMBigComplex) args[0];
      BigDecimal re = c.getRe();
      BigDecimal im = c.getIm();
      return Sqrt.sqrt(Addition.add(re.multiply(re), im.multiply(im)));
    }
    return Abs.abs((Number) args[0]);
  }
}
