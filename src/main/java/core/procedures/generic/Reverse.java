package core.procedures.generic;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.*;
import core.scm.Vector;

import java.util.*;

public final class Reverse extends AFn {

  public Reverse() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public String getName() {
    return "reverse";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof List) {
      Cons<Object> result = Cons.list();
      for (Object o : (List) arg) {
        result.push(o);
      }
      return result;
    }
    if (arg instanceof Set) {
      return Cons.list((Set)arg);
    }
    if (arg instanceof Map.Entry) {
      return new MapEntry(((Map.Entry) arg).getValue(), ((Map.Entry) arg).getKey());
    }
    if (arg instanceof Vector) {
      Object[] array = ((Vector) arg).getArray();
      MutableVector reversed = new MutableVector(Arrays.copyOf(array, array.length));
      Collections.reverse(Arrays.asList(reversed.getArray()));
      return reversed;
    }
    if (arg instanceof CharSequence) {
      return new StringBuilder((CharSequence) arg).reverse().toString();
    }
    throw new WrongTypeException(getName(), "List or Vector or Set or String", arg);
  }
}
