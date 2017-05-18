package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IAssoc;
import core.scm.MapEntry;
import core.utils.Utils;

public final class Find extends AFn {

  public Find() {
    super(new FnArgsBuilder().max(2).min(2).mandatory(new Class[]{IAssoc.class, Object.class}).build());
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
  public MapEntry apply2(Object map, Object key) {
    IAssoc assoc = Utils.INSTANCE.toAssoc(map);
    if (assoc.containsKey(key)) {
      return assoc.getEntry(key);
    }
    return null;
  }
}
