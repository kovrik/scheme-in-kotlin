package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitTest extends AFn {

  public BitTest() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[] {SCMClass.BitOp.class, Long.class}));
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
