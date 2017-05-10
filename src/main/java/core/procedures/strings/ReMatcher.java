package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ReMatcher extends AFn {

  public ReMatcher() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Pattern.class, CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "re-matcher";
  }

  @Override
  public Matcher apply2(Object arg1, Object arg2) {
    return ((Pattern) arg1).matcher((CharSequence) arg2);
  }
}
