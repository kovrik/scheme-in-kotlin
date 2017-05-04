package core.procedures.interop;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

public final class BooleanType extends AFn {

  public BooleanType() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "boolean";
  }

  @Override
  public Boolean apply1(Object arg) {
    /* Have to box it */
    if (Utils.toBoolean(arg)) {
      return Boolean.TRUE;
    } else {
      return Boolean.FALSE;
    }
  }
}
