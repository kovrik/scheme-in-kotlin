package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.cons.Append;
import core.procedures.cons.Car;
import core.procedures.cons.Cdr;
import core.procedures.generic.First;
import core.procedures.sets.SetProc;
import core.procedures.vectors.ListToVector;
import core.procedures.vectors.VectorToList;
import core.scm.Cons;
import core.scm.MutableVector;
import core.scm.Symbol;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import static core.procedures.cons.Append.Companion;
import static core.scm.Cons.*;
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

  public static final Symbol QUASIQUOTE_SYMBOL = Symbol.Companion.intern(QUASIQUOTE.toString());

  private final SetProc setProc = new SetProc();

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() != 2) {
      throw IllegalSyntaxException.Companion.of(toString(), expression);
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
    if (expr instanceof MutableVector) {
      /* Vector quasiquotation */
      return quasiquoteVector(expr, env, evaluator);
    } else if (expr instanceof Set) {
      /* Set quasiquotation */
      return quasiquoteSet(expr, env, evaluator);
    } else if (expr instanceof List) {
      List list = (List) expr;
      if (list.isEmpty()) {
        /* Nothing to process */
        return list;
      }
      /* Evaluate case when Quasiquote is immediately followed by Unquote: `,(+ 1 2) => 3 */
      if (isList(list) && (UNQUOTE_SYMBOL.equals(list.get(0)))) {
        if (list.size() != 2) {
          throw IllegalSyntaxException.Companion.of(UNQUOTE.toString(), expr, "unquote expects exactly one expression");
        }
        return evaluator.eval(list.get(1), env);
      }
      /* `,@(list 1 2) syntax is not valid */
      if (isList(list) && list.size() > 0 && (UNQUOTE_SPLICING_SYMBOL.equals(list.get(0)))) {
        throw IllegalSyntaxException.Companion.of(list.get(0).toString(), expr, "invalid context within quasiquote");
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
    boolean isList = (isList(list));
    Object result = list();
    for (int n = 0; n < list.size(); n++) {
      Object o = list.get(n);
      /* Append quoted forms recursively */
      if (!(o instanceof List) || (EMPTY.equals(o))) {
        /* Check special cases: `(1 unquote 2) => `(1 . 2) */
        if (n > 0 && UNQUOTE_SYMBOL.equals(o)) {
          /* if UNQUOTE is just before the last element a */
          if (n != list.size() - 2) {
            throw IllegalSyntaxException.Companion.of(UNQUOTE.toString(), list, "expects exactly one expression");
          }
          /* Evaluate and append last element */
          return Append.Companion.append(result, evaluator.eval(list.get(n + 1), env));
        }
        if (isList(expr) && UNQUOTE_SPLICING_SYMBOL.equals(o)) {
          throw IllegalSyntaxException.Companion
            .of(UNQUOTE_SPLICING.toString(), expr, "invalid context within quasiquote");
        }
        /* Otherwise, just append the element wrapped with LIST */
        result = Companion.append(result, list(o));
      } else {
        List el = (List) o;
        Object op = el.get(0);
        if (QUASIQUOTE_SYMBOL.equals(op)) {
          /* Increase depth of quasiquotation */
          result = Companion.append(result, list(quasiquoteList(depth + 1, o, env, evaluator)));
        } else if (UNQUOTE_SYMBOL.equals(op) || (UNQUOTE_SPLICING_SYMBOL.equals(op))) {
          if (el.size() != 2) {
            throw IllegalSyntaxException.Companion.of(op.toString(), expr, "expects exactly one expression");
          }
          if (depth == 0) {
            /* Level of quasiquotation is 0 - evaluate! */
            Object eval = evaluator.eval(el.get(1), env);
            if (UNQUOTE_SPLICING_SYMBOL.equals(op)) {
              /* Unquote Splicing: splice and append elements into resulting list */
              /* `(,@(list 1 2 3)) => `(1 2 3) */
              if (eval instanceof Collection) {
                ((List)result).addAll((Collection) eval);
              } else  {
                result = eval;
              }
            } else {
              /* Unquote: append list with results */
              /* `(,(list 1 2 3)) => `((1 2 3)) */
              result = Companion.append(result, list(eval));
            }
          } else {
            /* Decrease depth of quasiquotation */
            result = Companion.append(result, list(quasiquoteList(depth - 1, o, env, evaluator)));
          }
        } else {
          result = Companion.append(result, list(quasiquoteList(depth, o, env, evaluator)));
        }
      }
    }
    if (!isList) {
      /* In the case of a pair, if the cdr of the relevant quoted pair is empty,
       * then expr need not produce a list, and its result is used directly in place of the quoted pair */
      if ((isNull(Cdr.Companion.cdr(result)))) {
        return ((List)result).get(0);
      } else {
        // TODO Is car(cdr(result)) correct?
        return cons(Car.Companion.car(result), Car.Companion.car(Cdr.Companion.cdr(result)));
      }
    }
    return result;
  }

  // TODO Optimize vector->list and list-<vector conversions
  private Object quasiquoteVector(Object expr, Environment env, Evaluator evaluator) {
    MutableVector vector = (MutableVector) expr;
    if (vector.size() == 0) {
      /* Nothing to process */
      return vector;
    }
    /* `#(unquote 1)  syntax is not valid */
    /* `,@#(list 1 2) syntax is not valid */
    if (UNQUOTE_SYMBOL.equals(vector.get(0)) || UNQUOTE_SPLICING_SYMBOL.equals(vector.get(0))) {
      throw IllegalSyntaxException.Companion.of(vector.get(0).toString(), expr, "invalid context within quasiquote");
    }
    Cons list = VectorToList.Companion.vectorToList((MutableVector) expr);
    Object result = quasiquoteList(0, list, env, evaluator);
    // FIXME throw "illegal use of '.'" in Reader instead
    if (!isList(result)) {
      throw new IllegalSyntaxException("read: illegal use of '.'");
    }
    return ListToVector.Companion.listToVector(result);
  }

  private Object quasiquoteSet(Object expr, Environment env, Evaluator evaluator) {
    Set set = (Set) expr;
    if (set.size() == 0) {
      /* Nothing to process */
      return set;
    }
    /* `#(unquote 1)  syntax is not valid */
    /* `,@#(list 1 2) syntax is not valid */
    Object first = First.Companion.first(set);
    if (UNQUOTE_SYMBOL.equals(first) || UNQUOTE_SPLICING_SYMBOL.equals(first)) {
      throw IllegalSyntaxException.Companion.of(first.toString(), expr, "invalid context within quasiquote");
    }
    Cons list = Cons.list(set);
    Object result = quasiquoteList(0, list, env, evaluator);
    // FIXME throw "illegal use of '.'" in Reader instead
    if (!isList(result)) {
      throw new IllegalSyntaxException("read: illegal use of '.'");
    }
    return setProc.apply1(result);
  }

  @Override
  public String toString() {
    return "quasiquote";
  }
}
