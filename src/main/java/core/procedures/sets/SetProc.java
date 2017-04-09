package core.procedures.sets;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVector;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public final class SetProc extends AFn {

  public SetProc() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    if (arg instanceof Set) {
      return (Set<Object>) arg;
    }
    if (arg instanceof Collection) {
      return new HashSet<>((Collection)arg);
    }
    if (arg instanceof CharSequence) {
      Set<Object> set = new HashSet<>(((CharSequence)arg).length());
      ((CharSequence)arg).chars().forEach(c -> set.add((char)c));
      return set;
    }
    if (arg instanceof SCMVector) {
      return new HashSet<>(Arrays.asList(((SCMVector)arg).getArray()));
    }
    throw new WrongTypeException(getName(), "List or Vector or Set or String", arg);
  }
}
