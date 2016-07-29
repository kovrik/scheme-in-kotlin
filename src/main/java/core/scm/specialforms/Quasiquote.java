package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.scm.*;

import java.util.List;

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
/**
 * (define-syntax quasiquote
 *   (syntax-rules (unquote unquote-splicing)
 *     ((quasiquote (unquote datum))
 *       datum)
 *     ((quasiquote ((unquote-splicing datum) . next))
 *       (append datum (quasiquote next)))
 *     ((quasiquote (datum . next))
 *       (cons (quasiquote datum) (quasiquote next)))
 *     ((quasiquote datum)
 *       (quote datum))))
 */
public class Quasiquote implements ISpecialForm, ISCMClass {

  public static final Quasiquote QUASIQUOTE = new Quasiquote();

  private final String syntax = "quasiquote";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Quasiquote() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() != 2) {
      throw new IllegalSyntaxException("quasiquote: bad syntax");
    }
    return quasiquote(expression.get(1), env, evaluator);
  }

  private Object quasiquote(Object expr, IEnvironment env, IEvaluator evaluator) {
    if (expr instanceof SCMVector) {
      return quasiquoteVector(expr, env, evaluator);
    } else if (expr instanceof List) {
      return quasiquoteList(expr, env, evaluator);
    }
    /* (quasiquote datum) => (quote datum) */
    return expr;
  }

  private Object quasiquoteList(Object expr, IEnvironment env, IEvaluator evaluator) {

    List list = (List)expr;
    /* (quasiquote datum) => (quote datum) */
    if (list.isEmpty()) {
      return list;
    }
    /* (quasiquote (unquote datum)) => datum */
    Object o = list.get(0);
    if ((o instanceof SCMSymbol) && ("unquote".equals(((SCMSymbol) o).getValue()))) {
      if (list.size() != 2) {
        throw new IllegalSyntaxException("unquote: expects exactly one expression");
      }
      return evaluator.eval(list.get(1), env);
    }
    // TODO unquote-splicing

    /* (quasiquote (car . cdr)) => (cons (quasiquote car) (quasiquote cdr)) */
    Object car = quasiquote(Car.car(list), env, evaluator);
    Object cdr = quasiquote(Cdr.cdr(list), env, evaluator);
    return SCMCons.cons(car, cdr);
  }

  private Object quasiquoteVector(Object expr, IEnvironment env, IEvaluator evaluator) {

    SCMVector vector = (SCMVector)expr;
    /* (quasiquote datum) => (quote datum) */
    if (vector.length() == 0) {
      return vector;
    }
    /* (quasiquote (unquote datum)) => datum */
    Object o = vector.get(0);
    if ((o instanceof SCMSymbol) && ("unquote".equals(((SCMSymbol) o).getValue()))) {
      if (vector.length() != 2) {
        throw new IllegalSyntaxException("unquote: expects exactly one expression");
      }
      return evaluator.eval(vector.get(1), env);
    }
    // TODO unquote-splicing

    /* (quasiquote #(first . rest)) => #((quasiquote first) (quasiquote rest)) */
    Object car = quasiquote(vector.get(0), env, evaluator);
    SCMVector cdr = (SCMVector)quasiquote(vector.rest(), env, evaluator);

    SCMVector result = new SCMVector(vector.length());
    result.set(0, car);
    if (cdr.length() > 0) {
      for (int i = 1; i <= cdr.length(); i++) {
        result.set(i, cdr.get(i - 1));
      }
    }
    return result;
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
