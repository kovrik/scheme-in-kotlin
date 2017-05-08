package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.Cons;
import core.scm.Type;
import core.scm.Thunk;

public final class ForEach extends AFn {

  public ForEach() {
    super(new FnArgsBuilder().min(2).mandatory(new Class[]{IFn.class}).rest(Type.ProperList.class).build());
  }

  @Override
  public String getName() {
    return "for-each";
  }

  @Override
  public Thunk apply(Object... args) {
    /* For-each is the same as Map, but ignores the result */
    Thunk map = MapProc.MAP_PROC.apply(args);
    /* Void (ignore) results: (void <map-results>) */
    return new Thunk(Cons.list(VoidProc.VOID, map.getExpr()));
  }
}