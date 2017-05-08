package core.procedures.keywords;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Keyword;

public final class KeywordProc extends AFn {

  public KeywordProc() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "keyword";
  }

  @Override
  public Keyword apply1(Object arg) {
    return arg == null ? null : Keyword.intern(arg.toString());
  }
}
