package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMThunk;

import java.util.List;
import java.util.stream.IntStream;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (letrec <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
/*
 * TODO One restriction on letrec is very important:
 * it must be possible to evaluate each <init> without assigning or referring to the value of any <variable>.
 * If this restriction is violated, then it is an error.
 * The restriction is necessary because Scheme passes arguments by value rather than by name.
 * In the most common uses of letrec, all the <init>s are lambda expressions and the restriction is satisfied automatically.
 */
public enum LetRec implements ISpecialForm {
  LETREC;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    List<List> bindings = (List<List>)expression.get(1);
    /* Bind variables to fresh locations holding undefined values */
    Environment localEnv = new Environment(env);
    bindings.forEach(b -> localEnv.put(b.get(0), UNSPECIFIED));
    /* Evaluate inits */
    bindings.forEach(b -> localEnv.put(b.get(0), evaluator.eval(b.get(1), localEnv)));
    /* Evaluate body */
    IntStream.range(2, expression.size() - 1).forEach(i -> evaluator.eval(expression.get(i), localEnv));
    return new SCMThunk(expression.get(expression.size() - 1), localEnv);
  }

  @Override
  public String toString() {
    return "letrec";
  }
}
