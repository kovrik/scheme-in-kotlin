package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class StringToImmutableString extends AFn {

  public StringToImmutableString() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "string->immutable-string";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof String) {
      return arg;
    } else {
      return arg.toString();
    }
  }
}