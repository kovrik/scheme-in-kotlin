package core.procedures.vectors;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Vector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public final class Shuffle extends AFn {

  public Shuffle() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "shuffle";
  }

  @Override
  public Vector apply1(Object arg) {
    if (arg instanceof Collection) {
      ArrayList list = new ArrayList((Collection) arg);
      Collections.shuffle(list);
      return new Vector(list.toArray());
    }
    throw new WrongTypeException(getName(), Collection.class, arg);
  }
}
