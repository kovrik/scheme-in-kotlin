package core.procedures.keywords;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class Keyword extends AFn {

  public Keyword() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "keyword";
  }

  @Override
  public core.scm.Keyword apply1(Object arg) {
    return (arg == null) ? null : core.scm.Keyword.intern(arg.toString());
  }
}
