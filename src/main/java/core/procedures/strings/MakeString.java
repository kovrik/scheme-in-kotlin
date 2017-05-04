package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableString;
import core.scm.Type;

public final class MakeString extends AFn {

  public MakeString() {
    super(new FnArgsBuilder().min(1).max(2)
                             .mandatory(new Class[]{Type.ExactNonNegativeInteger.class})
                             .rest(Character.class).build());
  }

  @Override
  public String getName() {
    return "make-string";
  }

  @Override
  public MutableString apply(Object... args) {
    Long s = ((Number)args[0]).longValue();
    Object c = (args.length == 1) ? Character.MIN_VALUE : args[1];
    MutableString string = new MutableString();
    for (long i = 0; i < s; i++) {
      string.append(c);
    }
    return string;
  }
}
