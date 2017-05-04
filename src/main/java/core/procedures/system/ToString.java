package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.writer.Writer;

public class ToString extends AFn {

  public ToString() {
    super(new FnArgsBuilder().min(0).build());
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
    if (args.length == 1) {
      return str(args[0]);
    }
    StringBuilder sb = new StringBuilder();
    for (Object arg : args) {
      sb.append(str(arg));
    }
    return sb.toString();
  }

  private CharSequence str(Object obj) {
    if (obj == null) {
      return "";
    }
    if (obj instanceof Character) {
      return obj.toString();
    }
    if (obj instanceof CharSequence) {
      return (CharSequence) obj;
    }
    return Writer.write(obj);
  }
}
