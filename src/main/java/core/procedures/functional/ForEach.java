package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMThunk;

public final class ForEach extends AFn {

  public ForEach() {
    super(new FnArgsBuilder().minArgs(2).mandatoryArgsTypes(new Class[]{IFn.class})
                             .restArgsType(new Class[]{SCMClass.SCMProperList.class}));
  }

  @Override
  public String getName() {
    return "for-each";
  }

  @Override
  public SCMThunk apply(Object... args) {
    /* For-each is the same as Map, but ignores the result */
    SCMThunk map = MapProc.MAP_PROC.apply(args);
    /* Void (ignore) results: (void <map-results>) */
    return new SCMThunk(SCMCons.list(Void.VOID, map.getExpr()), null);
  }
}