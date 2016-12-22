package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public class StringToList extends AFn {

  @Override
  public String getName() {
    return "string->list";
  }

  @Override
  public SCMCons<Character> apply(Object... args) {
    Object o = args[0];
    SCMCons<Character> list = SCMCons.list();
    for (char c : (o.toString()).toCharArray()) {
      list.add(c);
    }
    return list;
  }
}