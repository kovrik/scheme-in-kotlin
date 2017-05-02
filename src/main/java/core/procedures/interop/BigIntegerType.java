package core.procedures.interop;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.math.BigInteger;

public final class BigIntegerType extends AFn {

  public BigIntegerType() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "bigint";
  }

  @Override
  public BigInteger apply1(Object arg) {
    if (arg instanceof Long) {
      return BigInteger.valueOf((long)arg);
    }
    return new BigInteger(arg.toString());
  }
}