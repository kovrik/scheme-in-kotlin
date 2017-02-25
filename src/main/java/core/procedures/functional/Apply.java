package core.procedures.functional;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.IFn;
import core.procedures.cons.Append;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.specialforms.Quote;
import core.scm.SCMThunk;

import java.util.Arrays;
import java.util.List;

public final class Apply extends AFn {

  public Apply() {
    super(new FnArgsBuilder().minArgs(2).mandatoryArgsTypes(new Class[]{IFn.class, Object.class})
                             .lastArgType(SCMClass.SCMProperList.class));
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
    for (Object o : (List) last) {
      sexp.add(SCMCons.list(Quote.QUOTE, o));
    }
    return new SCMThunk(sexp, null);
  }
}
