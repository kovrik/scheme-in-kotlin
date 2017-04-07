package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;
import core.scm.SCMVector;

import java.util.List;
import java.util.Map;

public final class Count extends AFn {

  public Count() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
      return ((Map)arg).size();
    } else if (arg instanceof List) {
      return ((List)arg).size();
    } else if (arg instanceof SCMVector) {
      return ((SCMVector)arg).length();
    } else if (arg instanceof String) {
      return ((String) arg).length();
    } else if (arg instanceof SCMMutableString)  {
      return ((SCMMutableString) arg).length();
    } else if (arg instanceof StringBuilder)  {
      return ((StringBuilder) arg).length();
    }
    throw new WrongTypeException(getName(), "List or Map or Vector or String", arg);
  }
}
