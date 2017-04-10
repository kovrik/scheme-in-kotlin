package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.SCMThunk;
import core.scm.SCMVoid;

import java.util.List;
import java.util.stream.IntStream;

/* Syntax:
 * (begin <expression1> <expression2> ...)
 *
 * NB:
 * Begin IGNORES first element of expression!
 *
 * In most cases it will be `begin` itself:
 *   (begin e1 e2 e3 ...)  ; begin is ignored
 *
 * But we also re-use Begin from some other Special Forms (do, cond, case).
 * In these cases we ignore first element as well:
 *   (else e1 e2 e3 ...)             ; else is ignored
 *   ((1 4 6 8 9) (quote composite)) ; first list is ignored - it was checked in `case` and evaluated to #t,
 *                                   ; so here we want to evaluate the rest of the form
 *
 * It may be more clear if we replace first elements with `begin` explicitly (in case, cond and do),
 * but it is ignored anyway, so why do one extra operation?
 */
public enum Begin implements ISpecialForm {
  BEGIN;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() <= 1) {
      return SCMVoid.VOID;
    }
    IntStream.range(1, expression.size() - 1).forEach(i -> evaluator.eval(expression.get(i), env));
    return new SCMThunk(expression.get(expression.size() - 1), env);
  }

  @Override
  public String toString() {
    return "begin";
  }
}
