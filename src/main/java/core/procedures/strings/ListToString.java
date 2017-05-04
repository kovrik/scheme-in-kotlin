package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Type;

import java.util.List;

public final class ListToString extends AFn {

  public ListToString() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Type.SCMProperList.class}).build());
  }

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
        throw new WrongTypeException(getName(), "Character", c);
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
