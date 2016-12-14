package core.procedures.lists;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.writer.Writer;

import java.util.List;

@FnArgs(args = {Object.class, List.class})
public class AssocProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public AssocProc(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object apply(Object... args) {
    Object obj = args[0];
    List list = (List) args[1];
    for (int n = 0; n < list.size(); n++) {
      Object pair = list.get(n);
      if (SCMCons.isPair(pair)) {
        if ((SCMBoolean.valueOf(predicate.apply(obj, ((SCMCons) pair).car())))) {
          return pair;
        }
      } else {
        throw new IllegalArgumentException(
          String.format("Wrong type argument in position %s (expecting association list): %s", n, Writer.write(list)));
      }
    }
    return SCMBoolean.FALSE;
  }

  @Override
  public String getName() {
    return name;
  }
}
