package main.core.evaluator;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.procedures.IFn;
import main.core.specialforms.SpecialForm;
import main.environment.IEnvironment;

import java.util.List;

public class Evaluator implements IEvaluator {

  public Object eval(Object sexp, IEnvironment env) {

    if (sexp instanceof SCMSymbol) {
      return env.find(sexp);
    } else if (sexp instanceof String && ((String) sexp).startsWith(",")) {
      if (",q".equals(sexp)) {
        System.out.println("Bye!");
        System.exit(0);
      }
    } else if (!(sexp instanceof List)) {
      return sexp;
    } else if (sexp instanceof SCMList) {

      SCMList<Object> list = (SCMList<Object>) sexp;
      if (list.isEmpty()) {
        throw new IllegalArgumentException("Unexpected syntax in form " + list);
      }
      Object op = list.get(0);
      if (op instanceof SpecialForm) {
        return ((SpecialForm)op).eval(list, env, this);
      }
      /* Function */
      Object fn = eval(op, env);
      if (fn == null) {
        throw new UnsupportedOperationException("Unbound variable: " + op);
      }
      Object[] args = new Object[list.size() - 1];
      for (int i = 1; i < list.size(); i++) {
        args[i - 1] = eval(list.get(i), env);
      }
      return ((IFn)fn).invoke(args);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
