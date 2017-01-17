package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.procedures.vectors.ListToVector;
import core.procedures.vectors.VectorToList;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMSymbol;

import java.util.List;

import static core.procedures.cons.Append.append;
import static core.scm.specialforms.Unquote.UNQUOTE;
import static core.scm.specialforms.Unquote.UNQUOTE_SYMBOL;
import static core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING;
import static core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL;

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
public enum Quasiquote implements ISpecialForm {
  QUASIQUOTE;

  public static final SCMSymbol QUASIQUOTE_SYMBOL = SCMSymbol.of(QUASIQUOTE.toString());

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() != 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    return quasiquote(expression.get(1), env, evaluator);
  }

  /**
   * Implement Quasiquotation using Append and List:
   *
   * 1. wrap each element, except for unquote-splicing forms, in a call to LIST
   * 2. APPEND the results
   * - quoted elements get processed recursively
   * - unquoted elements are passed to the call to LIST unprocessed
   * - unquote-splicing forms are inserted directly into the APPEND form
   *
   * http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
   */
  // TODO Simplify
  private Object quasiquote(Object expr, Environment env, Evaluator evaluator) {
    if (expr instanceof SCMMutableVector) {
      SCMMutableVector vector = (SCMMutableVector) expr;
      if (vector.length() == 0) {
        /* Nothing to process */
        return vector;
      }
      /* `#(unquote 1)  syntax is not valid */
      /* `,@#(list 1 2) syntax is not valid */
      if (UNQUOTE_SYMBOL.equals(vector.get(0)) || UNQUOTE_SPLICING_SYMBOL.equals(vector.get(0))) {
        throw IllegalSyntaxException.of(vector.get(0).toString(), expr, "invalid context within quasiquote");
      }
      /* Vector quasiquotation */
      return quasiquoteVector(0, expr, env, evaluator);
    } else if (expr instanceof List) {
      List list = (List) expr;
      if (list.isEmpty()) {
        /* Nothing to process */
        return list;
      }
      /* Evaluate case when Quasiquote is immediately followed by Unquote: `,(+ 1 2) => 3 */
      if (SCMCons.isList(list) && (UNQUOTE_SYMBOL.equals(list.get(0)))) {
        if (list.size() != 2) {
          throw IllegalSyntaxException.of(UNQUOTE.toString(), expr, "unquote expects exactly one expression");
        }
        return evaluator.eval(list.get(1), env);
      }
      /* `,@(list 1 2) syntax is not valid */
      if (SCMCons.isList(list) && list.size() > 0 && (UNQUOTE_SPLICING_SYMBOL.equals(list.get(0)))) {
        throw IllegalSyntaxException.of(list.get(0).toString(), expr, "invalid context within quasiquote");
      }
      /* List quasiquotation */
      return quasiquoteList(0, expr, env, evaluator);
    }
    /* (quasiquote datum) => (quote datum) */
    return expr;
  }

  // TODO Optimize and simplify
  private Object quasiquoteList(int depth, Object expr, Environment env, Evaluator evaluator) {
    List list = (List)expr;
    boolean isList = (SCMCons.isList(list));
    SCMCons result = SCMCons.list();
    for (int n = 0; n < list.size(); n++) {
      Object o = list.get(n);

      /* Append quoted forms recursively */
      if (!(o instanceof List) || (SCMCons.NIL.equals(o))) {
        /* Check special cases: `(1 unquote 2) => `(1 . 2) */
        if (n > 0 && UNQUOTE_SYMBOL.equals(o)) {
          /* if UNQUOTE is just before the last element a */
          if (n == list.size() - 2) {
            /* Evaluate and append last element */
            return append(result, evaluator.eval(list.get(n + 1), env));
          } else {
            throw IllegalSyntaxException.of(UNQUOTE.toString(), list, "expects exactly one expression");
          }
        }
        if (SCMCons.isList(expr) && UNQUOTE_SPLICING_SYMBOL.equals(o)) {
          throw IllegalSyntaxException.of(UNQUOTE_SPLICING.toString(), expr, "invalid context within quasiquote");
        }
        /* Otherwise, just append the element wrapped with LIST */
        result = (SCMCons) append(result, SCMCons.list(o));
      } else {
        List el = (List) o;
        Object op = el.get(0);
        if (QUASIQUOTE_SYMBOL.equals(op)) {
          /* Increase depth of quasiquotation */
          result = (SCMCons) append(result, SCMCons.list(quasiquoteList(depth + 1, o, env, evaluator)));
        } else if (UNQUOTE_SYMBOL.equals(op) || (UNQUOTE_SPLICING_SYMBOL.equals(op))) {
          if (el.size() != 2) {
            throw IllegalSyntaxException.of(op.toString(), expr, "expects exactly one expression");
          }
          if (depth == 0) {
            /* Level of quasiquotation is 0 - evaluate! */
            Object eval = evaluator.eval(el.get(1), env);
            if (UNQUOTE_SYMBOL.equals(op)) {
              /* Unquote: wrap result into a list */
              eval = SCMCons.list(eval);
            }
            result = (SCMCons) append(result, eval);
          } else {
            /* Decrease depth of quasiquotation */
            result = (SCMCons) append(result, SCMCons.list(quasiquoteList(depth - 1, o, env, evaluator)));
          }
        } else {
          result = (SCMCons) append(result, SCMCons.list(quasiquoteList(depth, o, env, evaluator)));
        }
      }
    }
    if (!isList) {
      /* In the case of a pair, if the cdr of the relevant quoted pair is empty,
       * then expr need not produce a list, and its result is used directly in place of the quoted pair */
      if ((SCMCons.isNull(Cdr.cdr(result)))) {
        return result.get(0);
      } else {
        // TODO Is car(cdr(result)) correct?
        return SCMCons.cons(Car.car(result), Car.car(Cdr.cdr(result)));
      }
    }
    return result;
  }

  // TODO Optimize vector->list and list-<vector conversions
  private Object quasiquoteVector(int depth, Object expr, Environment env, Evaluator evaluator) {
    SCMCons list = VectorToList.vectorToList((SCMMutableVector) expr);
    Object result = quasiquoteList(depth, list, env, evaluator);
    // FIXME throw "illegal use of '.'" in Reader instead
    if (!SCMCons.isList(result)) {
      throw new WrongTypeException("List", result);
    }
    return ListToVector.listToVector(result);
  }

  @Override
  public String toString() {
    return "quasiquote";
  }
}
