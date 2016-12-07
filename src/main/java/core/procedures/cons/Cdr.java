package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.ICons;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(args = {SCMCons.SCMPair.class})
public class Cdr extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cdr";
  }

  @Override
  public Object invoke(Object... args) {
    return cdr(args[0]);
  }

  public static Object cdr(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).cdr();
    }
    List list = (List) o;
    return list.subList(1, list.size());
  }
}
