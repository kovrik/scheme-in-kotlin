package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.SCMPair.class, SCMClass.ExactNonNegativeInteger.class})
public final class ListRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "list-ref";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    List list = (List)arg1;
    Long p = ((Number)arg2).longValue();
    if (p >= list.size()) {
      throw new IllegalArgumentException("Value out of range: " + p);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p == 0) {
        return ((SCMCons)list).car();
      } else {
        throw new IllegalArgumentException(String.format("%s: index (%s) reaches a non-pair", getName(), p));
      }
    }
    return list.get(p.intValue());
  }
}
