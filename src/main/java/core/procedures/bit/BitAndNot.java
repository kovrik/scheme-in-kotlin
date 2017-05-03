package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitAndNot extends AFn {

  public BitAndNot() {
    super(new FnArgsBuilder().minArgs(2).restArgsType(SCMClass.BitOp.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-and-not";
  }

  @Override
  public Long apply(Object... args) {
    long result = ((Number) args[0]).longValue();
    for (int i = 1; i < args.length; i++) {
      result = result & ~((Number)args[i]).longValue();
    }
    return result;
  }
}
