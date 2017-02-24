package core.procedures.lists;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBoolean;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.writer.Writer;

import java.util.List;

public final class AssocProc extends AFn {

  private final String name;
  /* Procedure used to compare objects for equality */
  private final AFn predicate;

  public AssocProc(String name, AFn predicate) {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{Object.class, SCMClass.SCMProperList.class}));
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    List list = (List) arg2;
    for (int n = 0; n < list.size(); n++) {
      Object pair = list.get(n);
      if (SCMCons.isPair(pair)) {
        if ((SCMBoolean.toBoolean(predicate.apply2(arg1, ((SCMCons) pair).car())))) {
          return pair;
        }
      } else {
        throw new IllegalArgumentException(
          String.format("%s: wrong type argument in position %s (expecting association list): %s", getName(), n, Writer.write(list)));
      }
    }
    return Boolean.FALSE;
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
