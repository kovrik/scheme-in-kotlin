package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.scm.SCMBoolean;
import core.writer.Writer;

import java.util.List;

public class MemberProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public MemberProc(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

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

  @Override
  public Object invoke(Object... args) {
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
