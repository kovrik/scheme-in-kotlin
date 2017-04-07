package core.procedures.hashmaps;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.generic.Count;
import core.procedures.generic.Nth;
import core.scm.SCMVector;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class Zipmap extends AFn {

  private final Count count = new Count();
  private final Nth nth = new Nth();

  public Zipmap() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "zipmap";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    if (!(arg1 instanceof SCMVector) && !(arg1 instanceof List)) {
      throw new WrongTypeException(getName(), "List or Vector", arg1);
    }
    if (!(arg1 instanceof SCMVector) && !(arg1 instanceof List)) {
      throw new WrongTypeException(getName(), "List or Vector", arg1);
    }
    int size = Math.min(count.apply1(arg1), count.apply1(arg2));
    Map<Object, Object> map = new HashMap<>();
    for (int i = 0; i < size; i++) {
      map.put(nth.apply(arg1, i), nth.apply(arg2, i));
    }
    return map;
  }
}
