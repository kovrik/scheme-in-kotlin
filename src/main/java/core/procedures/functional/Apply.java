package core.procedures.functional;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.cons.Append;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMThunk;
import core.scm.SCMVector;
import core.scm.specialforms.Quote;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public final class Apply extends AFn {

  public Apply() {
    super(new FnArgsBuilder().minArgs(2).mandatoryArgsTypes(new Class[]{IFn.class, Object.class}));
  }

  @Override
  public String getName() {
    return "apply";
  }

  @Override
  public Object apply(Object... args) {
    SCMCons sexp = SCMCons.list(args[0]);
    if (args.length > 2) {
      SCMCons<Object> list = SCMCons.list();
      list.addAll(Arrays.asList(args).subList(1, args.length - 1));
      sexp = (SCMCons) Append.append(sexp, list);
    }

    Object last = args[args.length - 1];
    if (!(last instanceof SCMVector) && !(last instanceof Collection)) {
      throw new WrongTypeException(getName(), "List or Vector or Set", last);
    }
    Iterator iter = null;
    if (last instanceof List) {
      iter = ((List) last).iterator();
    } else if (last instanceof SCMVector) {
      iter = ((SCMVector)last).iterator();
    } else if (last instanceof Set) {
      iter = ((Set)last).iterator();
    }
    while (iter.hasNext()) {
      Object o = iter.next();
      if ((o instanceof List) || (o instanceof SCMSymbol)) {
        sexp.add(Quote.quote(o));
      } else {
        sexp.add(o);
      }
    }
    return new SCMThunk(sexp);
  }
}
