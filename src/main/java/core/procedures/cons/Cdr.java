package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;
import core.scm.Type;

import java.util.List;

public class Cdr extends AFn {

  public Cdr() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.Pair.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cdr";
  }

  @Override
  public Object apply1(Object arg) {
    return cdr(arg);
  }

  public static Object cdr(Object o) {
    if (o instanceof Cons) {
      return ((Cons)o).cdr();
    }
    List list = (List) o;
    return list.subList(1, list.size());
  }
}
