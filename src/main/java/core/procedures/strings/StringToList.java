package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;
import core.scm.SCMString;

public class StringToList extends AFn {

  @Override
  public String getName() {
    return "string->list";
  }

  @Override
  public SCMCons<Character> invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    Object o = args[0];
    if (!(o instanceof String || o instanceof SCMString)) {
      throw new WrongTypeException("String", o);
    }
    SCMCons<Character> list = SCMCons.list();
    for (char c : (o.toString()).toCharArray()) {
      list.add(c);
    }
    return list;
  }
}