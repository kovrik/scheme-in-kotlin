package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;
import core.scm.MutableVector;

import java.util.List;

public final class ListToVector extends AFn {

  public ListToVector() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.ProperList.class}).build());
  }

  @Override
  public String getName() {
    return "list->vector";
  }

  @Override
  public MutableVector apply1(Object arg) {
    return listToVector(arg);
  }

  public static MutableVector listToVector(Object arg) {
    return new MutableVector(((List) arg).toArray());
  }
}
