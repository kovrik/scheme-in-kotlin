package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;

public final class BitTest extends AFn {

  public BitTest() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[] {Type.BitOp.class, Long.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-test";
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    long n = ((Number) arg2).longValue();
    return ((((Number)arg1).longValue() >> n) & 1L) == 1;
  }
}
