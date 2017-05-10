package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.regex.Pattern;

public final class RePattern extends AFn {

  public RePattern() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "re-pattern";
  }

  @Override
  public Pattern apply1(Object arg) {
    return Pattern.compile(arg.toString());
  }
}
