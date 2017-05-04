package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMThunk;

public final class ForEach extends AFn {

  public ForEach() {
    super(new FnArgsBuilder().min(2).mandatory(new Class[]{IFn.class})
                             .rest(SCMClass.SCMProperList.class).build());
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
    return new SCMThunk(SCMCons.list(Void.VOID, map.getExpr()));
  }
}