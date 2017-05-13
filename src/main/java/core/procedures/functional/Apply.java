package core.procedures.functional;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.scm.Cons;
import core.scm.Symbol;
import core.scm.Thunk;
import core.scm.specialforms.Quote;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public final class Apply extends AFn {

  public Apply() {
    super(new FnArgsBuilder().min(2).mandatory(new Class[]{IFn.class, Object.class}).build());
  }

  @Override
  public String getName() {
    return "apply";
  }

  @Override
  public Object apply(Object... args) {
    Cons sexp = Cons.list(args[0]);
    if (args.length > 2) {
      sexp.addAll(Arrays.asList(args).subList(1, args.length - 1));
    }

    Object last = args[args.length - 1];
    if (!(last instanceof Collection)) {
      throw new WrongTypeException(getName(), "List or Vector or Set", last);
    }
    for (Object o : ((Collection) last)) {
      if ((o instanceof List) || (o instanceof Symbol)) {
        sexp.add(Quote.quote(o));
      } else {
        sexp.add(o);
      }
    }
    return new Thunk(sexp);
  }
}
