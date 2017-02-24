package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.specialforms.Quote;
import core.scm.SCMThunk;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class MapProc extends AFn {

  static final MapProc MAP_PROC = new MapProc();

  public MapProc() {
    super(new FnArgsBuilder().minArgs(2).mandatoryArgsTypes(new Class[]{IFn.class})
                             .restArgsType(new Class[]{SCMClass.SCMProperList.class}));
  }

  @Override
  public String getName() {
    return "map";
  }

  @Override
  public SCMThunk apply(Object... args) {
    /* Check that all lists are of the same size */
    final int size = ((List)args[1]).size();
    boolean listsHaveSameSize = Arrays.stream(args).skip(1).allMatch(o -> ((List) o).size() == size);
    if (!listsHaveSameSize) {
      throw new IllegalArgumentException(String.format("%s: all lists must be of the same size", getName()));
    }

    // TODO Very naive implementation. Re-implement and optimize
    List<List> lists = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      lists.add(i, SCMCons.list(args[0]));
      for (int n = 1; n < args.length; n++) {
        lists.get(i).add(SCMCons.list(Quote.QUOTE, ((List)args[n]).get(i)));
      }
    }
    SCMCons<Object> result = SCMCons.list(SCMSymbol.of("list"));
    result.addAll(lists);
    /* Return Thunk that will be evaluated and produce results */
    return new SCMThunk(result, null);
  }
}
