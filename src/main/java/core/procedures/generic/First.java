package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.util.Iterator;

public class First extends AFn {

  public First() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "first";
  }

  @Override
  public Object apply1(Object arg) {
    return first(arg);
  }

  public static Object first(Object arg) {
    if (arg == null) {
      return null;
    }
    Iterator iterator = Utils.toIterator(arg);
    return iterator.hasNext() ? iterator.next() : null;
  }
}
