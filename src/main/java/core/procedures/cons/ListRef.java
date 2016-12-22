package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.SCMPair.class, SCMClass.ExactNonNegativeInteger.class})
public class ListRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "list-ref";
  }

  @Override
  public Object apply(Object... args) {
    Long p = ((Number)args[1]).longValue();
    List list = (List)args[0];
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
