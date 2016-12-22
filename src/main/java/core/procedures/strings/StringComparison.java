package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;

import java.util.function.BiPredicate;

@FnArgs(restArgsType = {String.class})
public class StringComparison extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  public static final StringComparison STRING_EQ          = new StringComparison("string=?",     String::equals);
  public static final StringComparison STRING_EQ_CI       = new StringComparison("string-ci=?",  String::equalsIgnoreCase);
  public static final StringComparison STRING_LE          = new StringComparison("string<?",     (arg1, arg2) -> arg1.compareTo(arg2) < 0);
  public static final StringComparison STRING_GR          = new StringComparison("string>?",     (arg1, arg2) -> arg1.compareTo(arg2) > 0);
  public static final StringComparison STRING_LE_OR_EQ    = new StringComparison("string<=?",    (arg1, arg2) -> arg1.compareTo(arg2) <= 0);
  public static final StringComparison STRING_GR_OR_EQ    = new StringComparison("string>=?",    (arg1, arg2) -> arg1.compareTo(arg2) >= 0);
  public static final StringComparison STRING_LE_CI       = new StringComparison("string-ci<?",  (arg1, arg2) -> arg1.toLowerCase().compareTo(arg2.toLowerCase()) < 0);
  public static final StringComparison STRING_GR_CI       = new StringComparison("string-ci>?",  (arg1, arg2) -> arg1.toLowerCase().compareTo(arg2.toLowerCase()) > 0);
  public static final StringComparison STRING_LE_OR_EQ_CI = new StringComparison("string-ci<=?", (arg1, arg2) -> arg1.toLowerCase().compareTo(arg2.toLowerCase()) <= 0);
  public static final StringComparison STRING_GR_OR_EQ_CI = new StringComparison("string-ci>=?", (arg1, arg2) -> arg1.toLowerCase().compareTo(arg2.toLowerCase()) >= 0);

  private final String name;
  private final BiPredicate<String, String> predicate;

  private StringComparison(String name, BiPredicate<String, String> predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object apply(Object... args) {
    if (args.length < 2) {
      return SCMBoolean.TRUE;
    }
    for (int i = 0; i < args.length - 1; i++) {
      if ((!predicate.test(args[i].toString(), args[i + 1].toString()))) {
        return SCMBoolean.FALSE;
      }
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public String getName() {
    return name;
  }
}
