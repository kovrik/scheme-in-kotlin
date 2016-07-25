package core.procedures.cons;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

public class Reverse extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, "reverse");
    }

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
