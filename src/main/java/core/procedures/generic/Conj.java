package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

import java.util.*;

public final class Conj extends AFn {

  public Conj() {
    super(new FnArgsBuilder().min(1).build());
  }

  @Override
  public String getName() {
    return "conj";
  }

  @Override
  public Object apply(Object... args) {
    Object first = args[0];
    if (args.length == 1) {
      return first;
    }
    if (first instanceof List) {
      SCMCons list = SCMCons.list();
      list.addAll((List)first);
      list.addAll(Arrays.asList(args).subList(1, args.length));
      return list;
    }
    if (first instanceof Set) {
      Set<Object> set = new HashSet<>();
      set.addAll((Collection)first);
      set.addAll(Arrays.asList(args).subList(1, args.length));
      return set;
    }
    if (first instanceof SCMVector) {
      int size = ((SCMVector) first).length() + args.length - 1;
      SCMVector vector = new SCMMutableVector(size, null);
      SCMVector v = (SCMVector)first;
      int length = v.length();
      for (int i = 0; i < length; i++) {
        vector.getArray()[i] = v.get(i);
      }
      System.arraycopy(args, 1, vector.getArray(), length, args.length - 1);
      return vector;
    }
    // TODO Map?
    throw new WrongTypeException(getName(), "List or Vector or Set", first);
  }
}
