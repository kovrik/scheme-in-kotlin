package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import core.scm.Vector;
import java.util.regex.Pattern;

public final class Split extends AFn {

  public Split() {
    super(new FnArgsBuilder().min(2).max(3).mandatory(new Class[]{CharSequence.class, Pattern.class}).rest(Long.class).build());
  }

  @Override
  public String getName() {
    return "split";
  }

  @Override
  public Vector apply(Object... args) {
    if (args.length == 2) {
      return new Vector(((Pattern)args[1]).split((CharSequence) args[0]));
    }
    return new Vector(((Pattern)args[1]).split((CharSequence) args[0], ((Long)args[2]).intValue()));
  }
}
