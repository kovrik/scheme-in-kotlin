package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

public class ListToString extends AFn {

  @Override
  public String getName() {
    return "list->string";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }

    Object o = args[0];
    if (!(o instanceof List) || ((o instanceof SCMCons) && !((SCMCons)o).isList())) {
      throw new WrongTypeException("List", o);
    }

    List cs = (List)o;
    if (cs.isEmpty()) {
      return "";
    }
    StringBuilder sb = new StringBuilder(cs.size());
    for (Object c : cs) {
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
