package core.procedures.hashmaps;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMapEntry;

import java.util.HashMap;
import java.util.Map;

// TODO Rename to Assoc?
public final class Put extends AFn {

  public Put() {
    super(new FnArgsBuilder().min(3).mandatory(new Class[]{Object.class, Object.class, Object.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "put";
  }

  @Override
  public Object apply(Object... args) {
    Object m = args[0];
    if (m instanceof Map) {
      if (args.length % 2 != 1) {
        throw new IllegalArgumentException(getName() + ": no value supplied for key: " + args[args.length - 1]);
      }
      Map<Object, Object> map = new HashMap((Map) args[0]);
      for (int i = 1; i < args.length; i = i + 2) {
        map.put(args[i], args[i + 1]);
      }
      return map;
    }
    if (m instanceof Map.Entry) {
      if (args.length > 3) {
        throw new IndexOutOfBoundsException(String.format("%s: value out of range", getName()));
      }
      return new SCMMapEntry(args[1], args[2]);
    }
    throw new WrongTypeException(getName(), "Map or MapEntry", m);
  }
}
