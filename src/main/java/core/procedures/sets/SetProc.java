package core.procedures.sets;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public final class SetProc extends AFn {

  public SetProc() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "set";
  }

  @Override
  public Set<Object> apply1(Object arg) {
    if (arg instanceof Collection) {
      return new HashSet<>((Collection)arg);
    }
    if (arg instanceof CharSequence) {
      Set<Object> set = new HashSet<>(((CharSequence)arg).length());
      CharSequence cs = ((CharSequence)arg);
      for (int i = 0; i < cs.length(); i++) {
        set.add(cs.charAt(i));
      }
      return set;
    }
    throw new WrongTypeException(getName(), "List or Vector or Set or String", arg);
  }
}
