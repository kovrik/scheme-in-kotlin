package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

import static core.scm.SCMConstant.UNSPECIFIED;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.SCMPair.class, Object.class})
public final class SetCdr extends AFn {

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
    return UNSPECIFIED;
  }
}