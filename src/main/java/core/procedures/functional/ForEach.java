package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.*;

@FnArgs(minArgs = 2, mandatoryArgsTypes = {IFn.class}, restArgsType = {SCMClass.SCMProperList.class})
public final class ForEach extends AFn {

  @Override
  public String getName() {
    return "for-each";
  }

  @Override
  public SCMThunk apply(Object... args) {
    /* For-each is the same as Map, but ignores the result */
    SCMThunk map = MapProc.MAP_PROC.apply(args);
    /* Void (ignore) results: (void <map-results>) */
    return new SCMThunk(SCMCons.list(Void.INSTANCE, map.getExpr()), null);
  }
}