package core.procedures.functional;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.cons.Append;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.specialforms.Quote;
import core.scm.SCMThunk;

import java.util.Arrays;
import java.util.List;

@FnArgs(minArgs = 2, mandatoryArgsTypes = {IFn.class, Object.class}, lastArgType = {SCMClass.SCMProperList.class})
public class Apply extends AFn {

  @Override
  public String getName() {
    return "apply";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length < 2) {
      throw new ArityException(args.length, getName());
    }
    SCMCons sexp = SCMCons.list(args[0]);
    if (args.length > 2) {
      SCMCons<Object> list = SCMCons.list();
      list.addAll(Arrays.asList(args).subList(1, args.length - 1));
      sexp = (SCMCons) Append.append(sexp, list);
    }

    Object last = args[args.length - 1];
    for (Object o : (List) last) {
      sexp.add(SCMCons.list(Quote.QUOTE, o));
    }
    return new SCMThunk(sexp, null);
  }
}
