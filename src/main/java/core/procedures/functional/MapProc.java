package core.procedures.functional;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.generic.Count;
import core.scm.Cons;
import core.scm.Symbol;
import core.scm.Thunk;
import core.scm.specialforms.Quote;
import core.utils.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public final class MapProc extends AFn {

  static final MapProc MAP_PROC = new MapProc();

  private final Count count = new Count();

  public MapProc() {
    super(new FnArgsBuilder().min(2).mandatory(new Class[]{IFn.class}).build());
  }

  @Override
  public String getName() {
    return "map";
  }

  // TODO Very naive implementation. Re-implement and optimize
  @Override
  public Thunk apply(Object... args) {
    /* Check that all lists/vectors are of the same size */
    final int size = count.apply1(args[1]);
    Map<Integer, Iterator> iterators = new HashMap<>(args.length - 1);
    for (int i = 1; i < args.length; i++) {
      /* Check type */
      iterators.put(i, Utils.INSTANCE.toSequence(args[i]));
      /* Check size */
      if (count.apply1(args[i]) != size) {
        throw new IllegalArgumentException(String.format("%s: all collections must be of the same size", getName()));
      }
    }

    List<List> lists = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      /* Add procedure as first element */
      lists.add(Cons.list(args[0]));
      /* Now add each Nth element of all lists */
      for (int n = 1; n < args.length; n++) {
        Object e = iterators.get(n).next();
        if ((e instanceof List) || (e instanceof Symbol)) {
          lists.get(i).add(Quote.quote(e));
        } else {
          lists.get(i).add(e);
        }
      }
    }
    Cons<Object> result = Cons.list(Symbol.intern("list"));
    result.addAll(lists);
    /* Return Thunk that will be evaluated and produce results */
    return new Thunk(result);
  }
}
