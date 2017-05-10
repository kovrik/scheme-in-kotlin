package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ReFind extends AFn {

  public ReFind() {
    super(new FnArgsBuilder().min(1).max(2).build());
  }

  @Override
  public String getName() {
    return "re-find";
  }

  @Override
  public String apply(Object... args) {
    if (args.length == 1) {
      if (!(args[0] instanceof Matcher)) {
        throw new WrongTypeException(getName(), Matcher.class, args[0]);
      }
      Matcher matcher = ((Matcher)args[0]);
      return matcher.find() ? matcher.group() : null;
    }
    if (!(args[0] instanceof Pattern)) {
      throw new WrongTypeException(getName(), Pattern.class, args[0]);
    }
    if (!(args[1] instanceof CharSequence)) {
      throw new WrongTypeException(getName(), CharSequence.class, args[1]);
    }
    Matcher matcher = ((Pattern)args[0]).matcher((CharSequence)args[1]);
    return matcher.find() ? matcher.group() : null;
  }
}
