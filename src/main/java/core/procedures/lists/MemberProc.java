package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.scm.SCMBoolean;
import core.writer.Writer;

import java.util.List;
import java.util.concurrent.ExecutionException;

public class MemberProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public MemberProc(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object arg1, Object arg2) {
    if (!(arg2 instanceof List)) {
      throw new IllegalArgumentException(
          String.format("Wrong type argument to `%s`! Expected: List, Actual: %s", getName(), Writer.write(arg2)));
    }
    List list = (List) arg2;
    if (list.isEmpty()) {
      return SCMBoolean.FALSE;
    }
    int p = 0;
    Object cons = list;
    while ((cons instanceof List) && (!((List) cons).isEmpty())) {
      p += 1;
      Object car = Car.car(cons);
      if ((SCMBoolean.valueOf(predicate.invoke(arg1, car)))) {
        return cons;
      }
      cons = Cdr.cdr(cons);
    }
    /* Not found */
    if (p == list.size()) {
      return SCMBoolean.FALSE;
    }
    throw new IllegalArgumentException(String.format("Wrong type argument in position %s (expecting list): %s",
                                                     p + 1, Writer.write(list)));
  }

// TODO Is this faster?
//  @Override
//  public Object invoke(Object arg1, Object arg2) {
//    if (!(arg2 instanceof List)) {
//      throw new IllegalArgumentException(
//          String.format("Wrong type argument to `%s`! Expected: List, Actual: %s", name, arg2));
//    }
//    List list = (List) arg2;
//    if (list.isEmpty()) {
//      return SCMBoolean.FALSE;
//    }
//    if (SCMCons.isList(list)) {
//      /* Proper list */
//      int i = -1;
//      for (int n = 0; n < list.size(); n++) {
//        Object o = list.get(n);
//        if ((Boolean) predicate.invoke(arg1, o)) {
//          i = n;
//          break;
//        }
//      }
//      /* Not found */
//      if (i == -1) {
//        return SCMBoolean.FALSE;
//      }
//      /* Return list, do not create a sublist */
//      if (i == 0) {
//        return list;
//      }
//      return list.subList(i, list.size());
//    } else {
//      /* Cons */
//      Object cons = list;
//      int p = 0;
//      while (SCMCons.isPair(cons)) {
//        p += 1;
//        Object car = Car.car(cons);
//        if ((Boolean) predicate.invoke(arg1, car)) {
//          return cons;
//        }
//        cons = Cdr.cdr(cons);
//      }
//      if (p == list.size()) {
//        return SCMBoolean.FALSE;
//      }
//      throw new IllegalArgumentException(
//          String.format("Wrong type argument in position %s (expecting list): %s", p + 1, list));
//    }
//  }

  @Override
  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length == 2) {
      return invoke(args[0], args[1]);
    }
    throw new ArityException(args.length, 2, getName());
  }

  @Override
  public String getName() {
    return name;
  }
}
