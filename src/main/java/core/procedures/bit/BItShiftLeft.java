package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BItShiftLeft extends AFn {

  public BItShiftLeft() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[] {SCMClass.BitOp.class, Long.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-shift-left";
  }

  @Override
  public Long apply2(Object arg1, Object arg2) {
    return ((Number) arg1).longValue() << ((Number)arg2).longValue();
  }
}
