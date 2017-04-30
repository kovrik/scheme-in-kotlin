package core.procedures.exceptions;

import core.exceptions.ExInfoException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;

public class ExData extends AFn {

  public ExData() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "ex-data";
  }

  @Override
  public Map apply1(Object arg) {
    if (arg instanceof ExInfoException) {
      return ((ExInfoException) arg).getInfo();
    }
    return null;
  }

}
