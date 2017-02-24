package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;

public final class StringCopy extends AFn {

  public StringCopy() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{String.class}));
  }

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public SCMMutableString apply1(Object arg) {
    return new SCMMutableString(arg.toString());
  }
}
