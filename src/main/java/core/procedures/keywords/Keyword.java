package core.procedures.keywords;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMKeyword;

public final class Keyword extends AFn {

  public Keyword() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "keyword";
  }

  @Override
  public SCMKeyword apply1(Object arg) {
    return (arg == null) ? null : SCMKeyword.intern(arg.toString());
  }
}
