package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(args = {List.class})
public class Reverse extends AFn {

  @Override
  public String getName() {
    return "reverse";
  }

  @Override
  public Object invoke(Object... args) {
    Object l = args[0];
    if (!SCMCons.isList(l)) {
      throw new WrongTypeException("List", l);
    }
    List list = (List)l;
    SCMCons<Object> result = SCMCons.list();
    for (Object o : list) {
      result.push(o);
    }
    return result;
  }
}
