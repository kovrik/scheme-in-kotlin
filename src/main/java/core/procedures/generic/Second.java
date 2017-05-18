package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.util.Iterator;

public class Second extends AFn {

  public Second() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "second";
  }

  @Override
  public Object apply1(Object arg) {
    return second(arg);
  }

  public static Object second(Object arg) {
    if (arg == null) {
      return null;
    }
    Iterator iterator = Utils.INSTANCE.toSequence(arg);
    if (iterator.hasNext()) {
      iterator.next();
      return iterator.hasNext() ? iterator.next() : null;
    }
    return null;
  }
}
