package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;
import core.scm.Cons;
import core.scm.Void;

import java.util.List;

public final class SetCdr extends AFn {

  public SetCdr() {
    super(new FnArgsBuilder().min(2).max(2)
                             .mandatory(new Class[]{Type.SCMPair.class, Object.class}).build());
  }

  @Override
  public String getName() {
    return "set-cdr!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    List list = (List)arg1;
    /* Remove tail */
    list.subList(1, list.size()).clear();
    /* Set new tail */
    if (arg2 instanceof List) {
      list.addAll((List) arg2);
    } else {
      list.add(arg2);
      if (list instanceof Cons) {
        ((Cons)list).setIsList(false);
      }
    }
    return Void.VOID;
  }
}