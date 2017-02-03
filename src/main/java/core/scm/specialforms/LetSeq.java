package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMThunk;

import java.util.List;
import java.util.stream.IntStream;

/* Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
public enum LetSeq implements ISpecialForm {
  LETSEQ;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    List<List> bindings = (List)expression.get(1);
    /* Evaluate inits */
    Environment localEnv = new Environment(env);
    bindings.forEach(b -> localEnv.put(b.get(0), evaluator.eval(b.get(1), localEnv)));
    /* Evaluate body */
    IntStream.range(2, expression.size() - 1).forEach(i -> evaluator.eval(expression.get(i), localEnv));
    return new SCMThunk(expression.get(expression.size() - 1), localEnv);
  }

  @Override
  public String toString() {
    return "let*";
  }
}
