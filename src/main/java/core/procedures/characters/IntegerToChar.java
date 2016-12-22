package core.procedures.characters;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Long.class})
public class IntegerToChar extends AFn {

  @Override
  public String getName() {
    return "integer->char";
  }

  @Override
  public Character apply(Object... args) {
    return (char)((Number)args[0]).longValue();
  }
}