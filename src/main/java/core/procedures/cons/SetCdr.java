package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

public final class SetCdr extends AFn {

  public SetCdr() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{SCMClass.SCMPair.class, Object.class}));
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
      if (list instanceof SCMCons) {
        ((SCMCons)list).setIsList(false);
      }
    }
    return null;
  }
}