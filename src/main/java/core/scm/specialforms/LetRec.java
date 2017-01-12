package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMThunk;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (letrec <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
/*
 * TODO:
 * One restriction on letrec is very important:
 * it must be possible to evaluate each <init> without assigning or referring to the value of any <variable>.
 * If this restriction is violated, then it is an error.
 * The restriction is necessary because Scheme passes arguments by value rather than by name.
 * In the most common uses of letrec, all the <init>s are lambda expressions and the restriction is satisfied automatically.
 */
public enum LetRec implements ISpecialForm {
  LETREC;

  private static final String syntax = "letrec";

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(syntax, expression);
    }

    Environment localEnv = new Environment(env);
    List<List> bindings = (List<List>)expression.get(1);
    /* Bind variables to fresh locations holding undefined values */
    for (List binding : bindings) {
      Object var = binding.get(0);
      localEnv.put(var, UNSPECIFIED);
    }
    /* Evaluate inits */
    for (List binding : bindings) {
      Object var  = binding.get(0);
      Object init = binding.get(1);
      localEnv.put(var, evaluator.eval(init, localEnv));
    }
    /* Evaluate body */
    for (int i = 2; i < expression.size() - 1; i++) {
      evaluator.eval(expression.get(i), localEnv);
    }
    return new SCMThunk(expression.get(expression.size() - 1), localEnv);
  }

  @Override
  public String toString() {
    return syntax;
  }
}
