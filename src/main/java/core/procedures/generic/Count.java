package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Collection;
import java.util.Map;

public class Count extends AFn {

  public Count() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "count";
  }

  @Override
  public Integer apply1(Object arg) {
    if (arg instanceof Map) {
      return ((Map) arg).size();
    } else if (arg instanceof Map.Entry) {
      return 2;
    } else if (arg instanceof Collection) {
      return ((Collection)arg).size();
    } else if (arg instanceof CharSequence) {
      return ((CharSequence) arg).length();
    }
    throw new WrongTypeException(getName(), "List or Map or Vector or Set or String", arg);
  }
}
