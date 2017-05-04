package core.procedures.cons;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;
import core.scm.Type;

import java.util.List;

public final class ListTail extends AFn {

  public ListTail() {
    super(new FnArgsBuilder().min(2).max(2)
                             .mandatory(new Class[]{Object.class, Type.ExactNonNegativeInteger.class}).build());
  }

  @Override
  public String getName() {
    return "list-tail";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    Long p = ((Number)arg2).longValue();
    if (p == 0) {
      return arg1;
    }
    if (!(arg1 instanceof List)) {
      throw new WrongTypeException(getName(), "List", arg1);
    }
    List list = (List) arg1;
    if (p >= list.size() + 1) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), p));
    }
    /* Cons cell */
    if ((list instanceof Cons) && !((Cons)list).isList()) {
      if (p == 1) {
        return ((Cons)list).cdr();
      } else {
        throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), p));
      }
    }
    return list.subList(p.intValue(), list.size());
  }
}
