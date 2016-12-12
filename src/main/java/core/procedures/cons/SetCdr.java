package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMClass.SCMPair.class, Object.class})
public class SetCdr extends AFn {

  @Override
  public String getName() {
    return "set-cdr!";
  }

  @Override
  public Object invoke(Object... args) {
    List list = (List)args[0];
    /* Remove tail */
    list.subList(1, list.size()).clear();
    /* Set new tail */
    Object o = args[1];
    if (o instanceof List) {
      list.addAll((List)o);
    } else {
      list.add(o);
      if (list instanceof SCMCons) {
        ((SCMCons)list).setIsList(false);
      }
    }
    return UNSPECIFIED;
  }
}