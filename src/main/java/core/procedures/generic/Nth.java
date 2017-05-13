package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

public final class Nth extends AFn {

  private final Count count = new Count();
  private final Get get = new Get();

  public Nth() {
    super(new FnArgsBuilder().min(2).max(3).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "nth";
  }

  @Override
  public Object apply(Object... args) {
    Object col = args[0];
    if (!Utils.isSeqable(col)) {
      throw new RuntimeException("Don't know how to create Sequence from " + col.getClass());
    }
    Object index = args[1];
    if (!Utils.isReal(index)) {
      throw new WrongTypeException(getName(), "Real", index);
    }
    int i = ((Number)args[1]).intValue();

    int size = count.apply1(col);
    if (size <= i && args.length < 3) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), i));
    }
    return get.apply(args);
  }
}
