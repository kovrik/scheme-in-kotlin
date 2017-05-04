package core.procedures.characters;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class IntegerToChar extends AFn {

  public IntegerToChar() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Long.class}).build());
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