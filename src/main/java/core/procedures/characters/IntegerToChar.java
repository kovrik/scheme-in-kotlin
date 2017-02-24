package core.procedures.characters;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class IntegerToChar extends AFn {

  public IntegerToChar() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Long.class}));
  }

  @Override
  public String getName() {
    return "integer->char";
  }

  @Override
  public Character apply1(Object arg) {
    return (char)((Number)arg).longValue();
  }
}