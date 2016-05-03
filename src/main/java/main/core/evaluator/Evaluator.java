package main.core.evaluator;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.procedures.IFn;
import main.core.procedures.Procedure;
import main.core.specialforms.SpecialForm;
import main.core.environment.Environment;
import main.core.environment.IEnvironment;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator implements IEvaluator {

  public Object eval(Object sexp, IEnvironment env) {

    if (sexp instanceof SCMSymbol) {
      /* Symbol */
      return env.find(sexp);
    } else if (sexp instanceof String && ((String) sexp).startsWith(",")) {
      /* Meta */
      if (",q".equals(sexp)) {
        System.out.println("Bye!");
        System.exit(0);
      }
    } else if (!(sexp instanceof List)) {
      return sexp;
    } else if (sexp instanceof SCMList) {

      SCMList<Object> list = (SCMList<Object>)sexp;
      if (list.isEmpty()) {
        throw new IllegalArgumentException("Unexpected syntax in form " + list);
      }
      Object op = list.getFirst();

      /* Special Form */
      if (op instanceof SpecialForm) {
        return ((SpecialForm)op).eval(list, env, this);
      }
      /* Function */
      Object fn = eval(op, env);
      if (fn == null) {
        throw new UnsupportedOperationException("Unbound variable: " + op);
      }
      /* Evaluate arguments */
      Object[] args = new Object[list.size() - 1];
      for (int i = 1; i < list.size(); i++) {
        args[i - 1] = eval(list.get(i), env);
      }

      /* Procedure */
      // FIXME Make generic as IFn?
      if (fn instanceof Procedure) {
        Procedure procedure = (Procedure) fn;
        List<Object> params = procedure.getParams();
        if (args.length != params.size()) {
          throw new IllegalArgumentException("Wrong number of arguments: expected " + params.size() +
              ", actual " + args.length);
        }
        Map<Object, Object> values = new HashMap<Object, Object>(params.size());
        for (int i = 0; i < params.size(); i++) {
          values.put(params.get(i), args[i]);
        }
        return eval(procedure.getBody(), new Environment(values, env));
      }

      return ((IFn)fn).invoke(args);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
