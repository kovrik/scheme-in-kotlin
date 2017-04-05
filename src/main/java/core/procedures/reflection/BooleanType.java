package core.procedures.reflection;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBoolean;

public final class BooleanType extends AFn {

  public BooleanType() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    // FIXME?
    /* Have to box it */
    if (SCMBoolean.toBoolean(arg)) {
      return Boolean.TRUE;
    } else {
      return Boolean.FALSE;
    }
  }
}
