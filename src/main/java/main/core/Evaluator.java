package main.core;

import main.ast.SCMList;
import main.ast.SCMSymbol;
import main.core.specialforms.SpecialForm;
import main.environment.Environment;
import main.core.procedures.IFn;

import java.util.List;

public class Evaluator implements IEvaluator {

  private static final Evaluator INSTANCE = new Evaluator();

  public Object eval(Object sexp, Environment env) {

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
      return ((IFn) fn).invoke(args);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }

  public static Evaluator getInstance() {
    return INSTANCE;
  }

}
