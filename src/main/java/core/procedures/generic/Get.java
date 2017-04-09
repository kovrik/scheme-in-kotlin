package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVector;
import core.utils.NumberUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;

public final class Get extends AFn {

  public Get() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(3));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "get";
  }

  @Override
  public Object apply(Object... args) {
    Object defaultValue = null;
    if (args.length == 3) {
      defaultValue = args[2];
    }
    return get(args[0], args[1], defaultValue);
  }

  private Object get(Object col, Object key, Object defaultValue) {
    if (col instanceof Map) {
      return ((Map)col).getOrDefault(key, defaultValue);
    } else if (col instanceof List) {
      if (NumberUtils.isInteger(key) && (((Number) key).intValue() < ((List) col).size())) {
        return ((List)col).get(((Number)key).intValue());
      }
    } else if (col instanceof Set) {
      if (((Set)col).contains(key)) {
        return key;
      }
    } else if (col instanceof SCMVector) {
      if (NumberUtils.isInteger(key) && (((Number) key).intValue() < ((SCMVector) col).length())) {
        return ((SCMVector)col).get(((Number)key).intValue());
      }
    } else if (col instanceof CharSequence) {
      if (NumberUtils.isInteger(key) && (((Number) key).intValue() < ((CharSequence) col).length())) {
        return ((CharSequence) col).charAt(((Number) key).intValue());
      }
    }
    return defaultValue;
  }
}
