package core.scm.specialforms;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMTailCall;

import java.util.List;

/* Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
public enum LetSeq implements ISpecialForm {
  LETSEQ;

  private static final String syntax = "let*";

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    Define.checkDefinitionContext(expression, env);

    IEnvironment localEnv = new Environment(env);
    List bindings = (List)expression.get(1);
    /* Evaluate inits */
    for (Object binding : bindings) {
      Object var  = ((List)binding).get(0);
      Object init = ((List)binding).get(1);
      localEnv.put(var, evaluator.eval(init, localEnv));
    }
    /* Evaluate body */
    for (int i = 2; i < expression.size() - 1; i++) {
      evaluator.eval(expression.get(i), localEnv);
    }
    /* Return Tail Call of the last expression */
    return new SCMTailCall(expression.get(expression.size() - 1), localEnv);
  }

  @Override
  public String toString() {
    return syntax;
  }
}
