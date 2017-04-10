package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.writer.Writer;

public final class ToString extends AFn {

  public ToString() {
    super(new FnArgsBuilder().minArgs(0).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "->string";
  }

  @Override
  public CharSequence apply(Object... args) {
    if (args.length == 0) {
      return "";
    }
    Object arg = args[0];
    if (arg == null) {
      return "";
    }
    if (arg instanceof Character) {
      return arg.toString();
    }
    if (arg instanceof CharSequence) {
      return (CharSequence) arg;
    }
    return Writer.write(arg);
  }
}
