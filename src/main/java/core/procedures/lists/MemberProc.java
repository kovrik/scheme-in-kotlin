package core.procedures.lists;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.scm.Cons;
import core.utils.Utils;
import core.writer.Writer;

import java.util.List;

public final class MemberProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public MemberProc(String name, AFn predicate) {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Object.class, List.class}).build());
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    List list = (List) arg2;
    if (list.isEmpty()) {
      return Boolean.FALSE;
    }
    int p = 0;
    Object cons = list;
    while ((cons instanceof List) && (!((List) cons).isEmpty())) {
      p += 1;
      Object car = Car.car(cons);
      if ((Utils.INSTANCE.toBoolean(predicate.apply2(arg1, car)))) {
        return cons;
      }
      cons = Cdr.cdr(cons);
    }
    /* Not found */
    if (p == list.size()) {
      if (!Cons.isList(list)) {
        throw new WrongTypeException(String.format("%s: wrong type argument in position %s (expecting list): %s",
                                                   getName(), p, Writer.write(list)));
      }
      return Boolean.FALSE;
    }
    throw new WrongTypeException(String.format("%s: wrong type argument in position %s (expecting list): %s",
                                               getName(), p + 1, Writer.write(list)));
  }

  @Override
  public Object apply(Object... args) {
    return apply2(args[0], args[1]);
  }

  @Override
  public String getName() {
    return name;
  }
}
