package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.util.Map;

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
    if (col instanceof Map) {
      throw new UnsupportedOperationException("nth not supported on this type: " + col.getClass());
    }
    if (!Utils.INSTANCE.isSeqable(col)) {
      throw new IllegalArgumentException("don't know how to create Sequence from " + col.getClass());
    }
    Object index = args[1];
    if (!Utils.INSTANCE.isInteger(index)) {
      throw new WrongTypeException(getName(), Integer.class, index);
    }
    int i = ((Number)args[1]).intValue();

    int size = count.apply1(col);
    if (size <= i && args.length < 3) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), i));
    }
    return get.apply(args);
  }
}
