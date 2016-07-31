package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.cons.IsList;
import core.scm.SCMVector;

import java.util.List;

public class ListToVector extends AFn {

  @Override
  public String getName() {
    return "list->vector";
  }

  @Override
  public SCMVector invoke(Object... args) {
    if (args != null && args.length == 1) {
      return listToVector(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }

  public static SCMVector listToVector(Object arg) {
    /* Must be a list, not a cons cell */
    if (IsList.isList(arg).toBoolean()) {
      List list = (List) arg;
      return new SCMVector(list.toArray());
    }
    throw new WrongTypeException("List", arg);
  }
}
