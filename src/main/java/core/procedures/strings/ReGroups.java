package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableVector;

import java.util.regex.Matcher;

public final class ReGroups extends AFn {

  public ReGroups() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Matcher.class}).build());
  }

  @Override
  public String getName() {
    return "re-groups";
  }

  @Override
  public Object apply1(Object arg) {
    Matcher m = (Matcher)arg;
    int gc = m.groupCount();
    if (gc == 0) {
      return m.group();
    }
    MutableVector result = new MutableVector(gc + 1, null);
    for (int c = 0; c <= gc; c++) {
      result.getArray()[c] = m.group(c);
    }
    return result;
  }
}
