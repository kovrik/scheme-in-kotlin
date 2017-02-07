package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.SCMBoolean;
import core.scm.SCMThunk;

import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;

/* Syntax:
 * (or <test1> ...)
 */
public enum Or implements ISpecialForm {
  OR;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() == 1) {
      return Boolean.FALSE;
    }
    Optional<Object> result = IntStream.range(1, expression.size() - 1)
                                        // evaluate each test (but last)
                                       .mapToObj(i -> evaluator.eval(expression.get(i), env))
                                        // find all successful tests
                                       .filter(SCMBoolean::toBoolean)
                                        // get first (if present)
                                       .findFirst();
    // return successful test (#t) or (if none) evaluate the last test
    return result.orElse(new SCMThunk(expression.get(expression.size() - 1), env));
  }

  @Override
  public String toString() {
    return "or";
  }
}
