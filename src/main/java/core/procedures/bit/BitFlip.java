package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitFlip extends AFn {

  public BitFlip() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[] {SCMClass.BitOp.class, Long.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-flip";
  }

  @Override
  public Long apply2(Object arg1, Object arg2) {
    long number = ((Number) arg1).longValue();
    return number ^ (1L << ((Number)arg2).longValue());
  }
}
