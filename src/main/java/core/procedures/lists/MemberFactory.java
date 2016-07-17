package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.procedures.math.IOperation;
import core.scm.SCMBoolean;
import core.scm.SCMCons;

import java.util.List;

public class MemberFactory {

  // TODO Cleanup
  public static AFn createMemberFn(String name, IOperation predicate) {
    return new AFn() {
      @Override
      public Object invoke(Object... args) {
        if (args != null && args.length == 2) {
          if (!(args[1] instanceof List)) {
            throw new IllegalArgumentException(
                String.format("Wrong type argument to `%s`! Expected: List, Actual: %s", name, args[1]));
          }
          List list = (List) args[1];
          int p = -1;
          if (SCMCons.isList(list)) {
            /* Proper list */
            int i = -1;
            for (int n = 0; n < list.size(); n++) {
              Object o = list.get(n);
              if ((Boolean)predicate.apply(args[0], o)) {
                i = n;
                break;
              }
            }
            if (i == -1) {
              return SCMBoolean.FALSE;
            }
            if (i == 0) {
              return list;
            }
            return list.subList(i, list.size());
          } else {
            /* Cons */
            Object cons = list;
            while (SCMCons.isPair(cons)) {
              p += 1;
              Object car = Car.car(cons);
              if ((Boolean)predicate.apply(args[0], car)) {
                return cons;
              }
              cons = Cdr.cdr(cons);
            }
            throw new IllegalArgumentException(
                String.format("Wrong type argument in position %s (expecting list): %s", p + 1, list));
          }
        }
        throw new ArityException(args.length, 2, name);
      }
    };
  }
}
