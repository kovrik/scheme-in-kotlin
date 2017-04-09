package core.procedures.functional;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.generic.Count;
import core.procedures.generic.Get;
import core.scm.*;
import core.scm.specialforms.Quote;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public final class MapProc extends AFn {

  static final MapProc MAP_PROC = new MapProc();

  private final Count count = new Count();
  private final Get get = new Get();

  public MapProc() {
    super(new FnArgsBuilder().minArgs(2).mandatoryArgsTypes(new Class[]{IFn.class}));
  }

  @Override
  public String getName() {
    return "map";
  }

  // TODO Very naive implementation. Re-implement and optimize
  @Override
  public SCMThunk apply(Object... args) {
    /* Check that all lists/vectors are of the same size */
    final int size = count.apply1(args[1]);
    for (int i = 1; i < args.length; i++) {
      /* Check type */
      if (!(args[i] instanceof SCMVector) && !(args[i] instanceof Collection) && !(args[i] instanceof CharSequence)) {
        throw new WrongTypeException(getName(), "List or Vector or Set", args[i]);
      }
      /* Check size */
      if (count.apply1(args[i]) != size) {
        throw new IllegalArgumentException(String.format("%s: all collections must be of the same size", getName()));
      }
    }

    List<List> lists = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      lists.add(i, SCMCons.list(args[0]));
      for (int n = 1; n < args.length; n++) {
        Object list = args[n];
        if (list instanceof Set) {
          list = new ArrayList<>((Set)list);
        }
        Object e = get.apply(list, i);
        lists.get(i).add(SCMCons.list(Quote.QUOTE, e));
      }
    }
    SCMCons<Object> result = SCMCons.list(SCMSymbol.of("list"));
    result.addAll(lists);
    /* Return Thunk that will be evaluated and produce results */
    return new SCMThunk(result, null);
  }
}
