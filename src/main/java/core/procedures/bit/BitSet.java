package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitSet extends AFn {

  public BitSet() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[] {SCMClass.BitOp.class, Long.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-set";
  }

  @Override
  public Long apply2(Object arg1, Object arg2) {
    long number = ((Number) arg1).longValue();
    return number | (1L << ((Number)arg2).longValue());
  }
}
