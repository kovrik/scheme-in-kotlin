package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MapEntry;

import java.util.Map;

public final class Find extends AFn {

  public Find() {
    super(new FnArgsBuilder().max(2).min(2).mandatory(new Class[]{Map.class, Object.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "find";
  }

  @Override
  public Map.Entry apply2(Object map, Object key) {
    Map<Object, Object> hashMap = (Map)map;
    if (hashMap.containsKey(key)) {
      return new MapEntry(key, hashMap.get(key));
    }
    return null;
  }
}
