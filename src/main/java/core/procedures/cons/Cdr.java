package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.ICons;
import core.scm.Type;

import java.util.List;

public class Cdr extends AFn {

  public Cdr() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.SCMPair.class}).build());
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
    if (o instanceof ICons) {
      return ((ICons)o).cdr();
    }
    List list = (List) o;
    return list.subList(1, list.size());
  }
}
