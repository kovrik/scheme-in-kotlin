package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public final class StringToList extends AFn {

  @Override
  public String getName() {
    return "string->list";
  }

  @Override
  public SCMCons<Character> apply1(Object arg) {
    SCMCons<Character> list = SCMCons.list();
    for (char c : (arg.toString()).toCharArray()) {
      list.add(c);
    }
    return list;
  }
}