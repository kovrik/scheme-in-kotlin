package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;

public final class StringToMutableString extends AFn {

  public StringToMutableString() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "string->mutable-string";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof SCMMutableString || arg instanceof StringBuilder) {
      return arg;
    } else {
      return new SCMMutableString(arg.toString());
    }
  }
}