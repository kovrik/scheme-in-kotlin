package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Arrays;

public final class StringAppend extends AFn {

  public StringAppend() {
    super(new FnArgsBuilder().restArgsType(String.class));
  }

  @Override
  public String getName() {
    return "string-append";
  }

  @Override
  public String apply(Object... args) {
    if (args.length == 0) {
      return "";
    }
    if (args.length == 1) {
      Object o = args[0];
      return o.toString();
    }
    StringBuilder sb = new StringBuilder();
    Arrays.stream(args).forEach(sb::append);
    return sb.toString();
  }
}
