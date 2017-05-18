package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.Utils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public final class Zipmap extends AFn {

  public Zipmap() {
    super(new FnArgsBuilder().min(2).max(2).build());
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
    Iterator iterator1 = Utils.INSTANCE.toSequence(arg1);
    Iterator iterator2 = Utils.INSTANCE.toSequence(arg2);
    Map<Object, Object> map = new HashMap<>();
    while (iterator1.hasNext() && iterator2.hasNext()) {
      map.put(iterator1.next(), iterator2.next());
    }
    return map;
  }
}
