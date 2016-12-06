package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(args = {String.class})
public class StringToList extends AFn {

  @Override
  public String getName() {
    return "string->list";
  }

  @Override
  public SCMCons<Character> invoke(Object... args) {
    Object o = args[0];
    SCMCons<Character> list = SCMCons.list();
    for (char c : (o.toString()).toCharArray()) {
      list.add(c);
    }
    return list;
  }
}