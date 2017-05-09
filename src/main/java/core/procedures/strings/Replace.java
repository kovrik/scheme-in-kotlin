package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.regex.Pattern;

public final class Replace extends AFn {

  public Replace() {
    super(new FnArgsBuilder().min(3).max(3).build());
  }

  @Override
  public String getName() {
    return "replace";
  }

  @Override
  public String apply3(Object arg1, Object arg2, Object arg3) {
    if (!(arg1 instanceof CharSequence)) {
      throw new WrongTypeException(getName(), "String", arg1);
    }
    if (arg2 instanceof Character && arg3 instanceof Character) {
      return arg1.toString().replace((Character) arg2, (Character)arg3);
    }
    if (arg2 instanceof CharSequence && arg3 instanceof CharSequence) {
      return arg1.toString().replace((CharSequence) arg2, (CharSequence)arg3);
    }
    // TODO arg2=string/arg3=function of match
    return ((Pattern)arg2).matcher((CharSequence)arg1).replaceAll(((CharSequence)arg3).toString());
  }
}
