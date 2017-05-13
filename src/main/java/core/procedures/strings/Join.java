package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.util.Iterator;

public final class Join extends AFn {

  public Join() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "join";
  }

  @Override
  public String apply(Object... args) {
    if (args.length == 1) {
      return args[0].toString();
    }
    String separator = args[0].toString();
    Iterator iterator = Utils.toIterator(args[1]);
    if (!iterator.hasNext()) {
      return "";
    }
    StringBuilder sb = new StringBuilder();
    while (iterator.hasNext()) {
      sb.append(iterator.next());
      if (iterator.hasNext()) {
        sb.append(separator);
      }
    }
    return sb.toString();
  }
}
