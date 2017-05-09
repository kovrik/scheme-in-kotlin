package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.regex.Pattern;

public final class ReplaceFirst extends AFn {

  public ReplaceFirst() {
    super(new FnArgsBuilder().min(3).max(3).build());
  }

  @Override
  public String getName() {
    return "replace-first";
  }

  @Override
  public String apply3(Object arg1, Object arg2, Object arg3) {
    if (!(arg1 instanceof CharSequence)) {
      throw new WrongTypeException(getName(), "String", arg1);
    }
    if (arg2 instanceof CharSequence && arg3 instanceof CharSequence) {
      return arg1.toString().replaceFirst(arg2.toString(), arg3.toString());
    }
    // TODO arg2=string/arg3=function of match
    return ((Pattern)arg2).matcher((CharSequence)arg1).replaceFirst(((CharSequence)arg3).toString());
  }
}
