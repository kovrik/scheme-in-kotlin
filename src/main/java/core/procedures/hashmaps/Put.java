package core.procedures.hashmaps;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MapEntry;
import core.scm.MutableVector;
import core.utils.Utils;

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
    if (args.length > 3) {
      throw new ArityException(getName(), 2, 2, args.length);
    }
    if (m instanceof Map.Entry) {
      m = new MapEntry((Map.Entry) m);
    }
    if (m instanceof MutableVector) {
      if (!Utils.isInteger(args[1])) {
        throw new WrongTypeException("vector", Integer.class, args[1]);
      }
      ((MutableVector) m).set(((Number)args[1]).intValue(), args[2]);
      return m;
    }
    throw new WrongTypeException(getName(), "MutableVector or Map or MapEntry", m);
  }
}
