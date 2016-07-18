package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.writer.Writer;

import java.util.List;

public class AssocProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public AssocProc(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[1] instanceof List)) {
        throw new IllegalArgumentException(
            String.format("Wrong type argument to `%s`! Expected: List, Actual: %s", getName(), Writer.write(args[1])));
      }
      Object obj = args[0];
      List list = (List)args[1];
      for (int n = 0; n < list.size(); n++) {
        Object pair = list.get(n);
        if (SCMCons.isPair(pair)) {
          if ((SCMBoolean.valueOf(predicate.invoke(obj, ((SCMCons)pair).car())))) {
            return pair;
          }
        } else {
          throw new IllegalArgumentException(
              String.format("Wrong type argument in position %s (expecting association list): %s", n, Writer.write(list)));
        }
      }
      return SCMBoolean.FALSE;
    }
    throw new ArityException(args.length, 2, getName());
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
