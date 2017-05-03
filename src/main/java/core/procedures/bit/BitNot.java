package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitNot extends AFn {

  public BitNot() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[] {SCMClass.BitOp.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-not";
  }

  @Override
  public Long apply1(Object arg) {
    return ~((Number)arg).longValue();
  }
}
