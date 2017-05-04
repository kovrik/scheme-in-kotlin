package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitNot extends AFn {

  public BitNot() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[] {SCMClass.BitOp.class}).build());
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
