package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public final class Substring extends AFn {

  public Substring() {
    super(new FnArgsBuilder().min(2).max(3)
                             .mandatory(new Class[]{CharSequence.class, SCMClass.ExactNonNegativeInteger.class})
                             .rest(SCMClass.ExactNonNegativeInteger.class).build());
  }

  @Override
  public String getName() {
    return "substring";
  }

  @Override
  public String apply(Object... args) {
    String s = args[0].toString();
    long start = ((Number)args[1]).longValue();
    if (start > s.length()) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), start));
    }

    long end = s.length();
    if (args.length == 3) {
      end = (long) args[2];
    }
    if (end > s.length()) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), end));
    }
    return s.substring((int)start, (int)end);
  }
}
