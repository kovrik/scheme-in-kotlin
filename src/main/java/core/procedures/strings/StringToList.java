package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;

public final class StringToList extends AFn {

  public StringToList() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
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