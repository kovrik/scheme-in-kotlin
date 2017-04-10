package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMapEntry;

import java.util.HashMap;
import java.util.Map;

public final class Find extends AFn {

  public Find() {
    super(new FnArgsBuilder().maxArgs(2).minArgs(2).mandatoryArgsTypes(new Class[]{Map.class, Object.class}));
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
    Map<Object, Object> hashMap = new HashMap((Map)map);
    if (hashMap.containsKey(key)) {
      return new SCMMapEntry(key, hashMap.get(key));
    }
    return null;
  }
}
