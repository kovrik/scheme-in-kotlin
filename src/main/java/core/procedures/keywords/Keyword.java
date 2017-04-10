package core.procedures.keywords;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMKeyword;

public final class Keyword extends AFn {

  public Keyword() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "keyword";
  }

  @Override
  public SCMKeyword apply1(Object arg) {
    return (arg == null) ? null : SCMKeyword.of(arg.toString());
  }
}
