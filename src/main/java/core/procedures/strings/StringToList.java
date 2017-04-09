package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;

public final class StringToList extends AFn {

  public StringToList() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

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