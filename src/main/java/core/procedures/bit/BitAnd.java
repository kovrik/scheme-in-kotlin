package core.procedures.bit;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;

public final class BitAnd extends AFn {

  public BitAnd() {
    super(new FnArgsBuilder().min(2).rest(Type.BitOp.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bit-and";
  }

  @Override
  public Long apply(Object... args) {
    long result = ((Number) args[0]).longValue();
    for (int i = 1; i < args.length; i++) {
      result &= ((Number)args[i]).longValue();
    }
    return result;
  }
}
