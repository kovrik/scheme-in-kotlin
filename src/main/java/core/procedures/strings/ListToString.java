package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.SCMProperList.class})
public final class ListToString extends AFn {

  @Override
  public String getName() {
    return "list->string";
  }

  @Override
  public Object apply1(Object arg) {
    List cs = (List)arg;
    if (cs.isEmpty()) {
      return "";
    }
    StringBuilder sb = new StringBuilder(cs.size());
    for (Object c : cs) {
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
