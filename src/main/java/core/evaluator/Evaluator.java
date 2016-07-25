package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;
import core.writer.Writer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator implements IEvaluator {

  @Override
  public Object eval(Object sexp, IEnvironment env) {
    if (sexp instanceof SCMSymbol) {
      /* Symbol */
      if (((SCMSymbol) sexp).getValue().startsWith(",")) {
        /* Meta */
        return evmeta(((SCMSymbol) sexp).getValue());
      }
      /* Check if it is a Special Form */
      Object o = env.find(sexp);
      if (o instanceof ISpecialForm) {
        throw new IllegalSyntaxException("Unexpected syntax in form: " + o);
      }
      return o;
    } else if (!(sexp instanceof List)) {
      return sexp;
    } else if (sexp instanceof List) {
      return evlis(sexp, env);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }

  /**
   * Evaluate a list
   */
  private Object evlis(Object sexp, IEnvironment env) {
    List list = (List)sexp;
    if (list.isEmpty()) {
      throw new IllegalSyntaxException("Unexpected syntax in form " + list);
    }
    /* Check if op is a Special Form */
    Object op = list.get(0);
    if (op instanceof SCMSymbol) {
      /* Get it from the environment: let user redefine special forms */
      Object specialForm = env.find(op);
      if (specialForm instanceof ISpecialForm) {
        return ((ISpecialForm)specialForm).eval(list, env, this);
      }
    }

    /* Must be a procedure */
    Object fn = eval(op, env);
    if (!(fn instanceof IFn)) {
      throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(fn));
    }

    /* Evaluate arguments */
    List<Object> args = new ArrayList<Object>(list.size() - 1);
    for (int i = 1; i < list.size(); i++) {
      args.add(eval(list.get(i), env));
    }

    // FIXME Make generic as IFn?
    if (fn instanceof SCMProcedure) {
      IEnvironment closure = ((SCMProcedure)fn).getClosure();
      closure = (closure == null) ? env : closure;
      return apply((SCMProcedure)fn, args, closure);
    }
    return ((IFn)fn).invoke(args.toArray());
  }

  private Object apply(SCMProcedure fn, List<Object> args, IEnvironment env) {
    List<SCMSymbol> params = fn.getParams();
    /* Check arity */
    if (!fn.isVariableArity() && (args.size() != params.size())) {
      throw new ArityException(args.size(), params.size(), fn.getName());
    }

    Map<Object, Object> values = new HashMap<Object, Object>(params.size());
    if (!fn.isVariableArity()) {
      for (int i = 0; i < params.size(); i++) {
        values.put(params.get(i), args.get(i));
      }
    } else {
      /* Variadic arity procedure */
      /* Check arity */
      if (args.size() < params.size() - 1) {
        throw new ArityException(args.size(), fn.getName());
      }
      /* Put mandatory params first */
      for (int i = 0; i < params.size() - 1; i++) {
        values.put(params.get(i), args.get(i));
      }
      /* Then rest */
      List<Object> varargs = SCMCons.list(args.subList(params.size() - 1, args.size()));
      values.put(params.get(params.size() - 1), varargs);
    }
    return evlis(fn.getBody(), new Environment(values, env));
  }

  /**
   * Evaluate meta
   */
  private Object evmeta(String sexp) {
    if (",q".equals(sexp)) {
      System.out.println("Bye!");
      System.exit(0);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
