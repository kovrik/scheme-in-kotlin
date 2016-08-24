package core.scm.specialforms.tco;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;

import java.util.List;

import static core.scm.specialforms.tco.Lambda.LAMBDA;
import static core.scm.specialforms.tco.LetRec.LETREC;

/* Syntax:
 * (let <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
public class Let implements ISpecialForm, ISCMClass {

  public static final Let LET = new Let();

  private final String syntax = "let";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Let() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 3) {
      throw new IllegalSyntaxException("let: bad let in form: " + expression);
    }
    /* Normal let:
     * (let ((id expr) ...) body ...+) */
    if (expression.get(1) instanceof List) {
      IEnvironment localEnv = new Environment(env);
      /* Evaluate inits */
      List bindings = (List) expression.get(1);
      for (Object binding : bindings) {
        Object var  = ((List)binding).get(0);
        Object init = ((List)binding).get(1);
        if (localEnv.get(var) != null) {
          throw new IllegalSyntaxException("let: duplicate identifier: " + var);
        }
        localEnv.put(var, evaluator.eval(init, env));
      }

      /* Evaluate body */
      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      /* Return Tail Call of the last expression */
      return new TailCall(expression.get(expression.size() - 1), localEnv);

    } else if (expression.get(1) instanceof SCMSymbol) {
      // TODO Optimize and cleanup
      /* Named let:
       * (let proc-id ((arg-id init-expr) ...) body ...+) */
      Object o = expression.get(1);
      if (!(o instanceof SCMSymbol)) {
        throw new IllegalSyntaxException("let: bad let in form: " + expression);
      }
      /* Construct lambda */
      SCMCons<Object> lambdaArgs = SCMCons.list();
      SCMCons<Object> initValues = SCMCons.list();
      List bindings = (List)expression.get(2);
      for (Object binding : bindings) {
        Object arg = ((List)binding).get(0);
        if (lambdaArgs.contains(arg)) {
          throw new IllegalSyntaxException("let: duplicate identifier: " + arg);
        }
        lambdaArgs.add(arg);
        initValues.add(((List)binding).get(1));
      }
      Object lambdaBody = expression.get(3);
      SCMCons lambda = SCMCons.list(LAMBDA, lambdaArgs, lambdaBody);
      SCMSymbol name = (SCMSymbol)o;
      SCMCons<SCMCons> l = SCMCons.list();
      l.add(SCMCons.list(name, lambda));

      SCMCons<Object> body = SCMCons.list(name);
      body.addAll(initValues);

      /* Named let is implemented via letrec */
      SCMCons<Object> letrec = SCMCons.list(LETREC);
      letrec.add(l);
      letrec.add(body);
      /* Letrec has TCO */
      return LETREC.eval(letrec, new Environment(env), evaluator);
    }
    throw new IllegalSyntaxException("let: bad let in form: " + expression);
  }

  public SCMSymbol symbol() {
    return symbol;
  }

  @Override
  public String toString() {
    return syntax;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
