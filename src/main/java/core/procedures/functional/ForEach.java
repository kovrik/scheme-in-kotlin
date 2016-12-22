package core.procedures.functional;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.specialforms.Quote;
import core.scm.SCMTailCall;

import java.util.ArrayList;
import java.util.List;

@FnArgs(minArgs = 2, mandatoryArgsTypes = {IFn.class}, restArgsType = {SCMClass.SCMProperList.class})
public class ForEach extends AFn {

  @Override
  public String getName() {
    return "for-each";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length < 2) {
      throw new ArityException(args.length, getName());
    }
    SCMCons<Object> result = SCMCons.list(new SCMSymbol("list"));

    int size = -1;
    /* Check lists and their sizes */
    for (int i = 1; i < args.length; i++) {
      List l = (List)args[i];
      if (size == -1) {
        size = l.size();
      }
      if (l.size() != size) {
        throw new IllegalArgumentException(String.format("%s: all lists must be of the same size", getName()));
      }
    }

    // TODO Very naive implementation. Re-implement and optimize
    List<List> lists = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      lists.add(i, SCMCons.list(args[0]));
      for (int n = 1; n < args.length; n++) {
        lists.get(i).add(SCMCons.list(Quote.QUOTE, ((List)args[n]).get(i)));
      }
    }
    result.addAll(lists);

    /* Void results */
    result = SCMCons.list(Void.INSTANCE, result);
    return new SCMTailCall(result, null);
  }
}