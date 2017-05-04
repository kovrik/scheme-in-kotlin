package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class BitXor extends AFn {

  public BitXor() {
    super(new FnArgsBuilder().min(2).rest(SCMClass.BitOp.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-xor";
  }

  @Override
  public Long apply(Object... args) {
    long result = ((Number) args[0]).longValue();
    for (int i = 1; i < args.length; i++) {
      result ^= ((Number)args[i]).longValue();
    }
    return result;
  }
}
