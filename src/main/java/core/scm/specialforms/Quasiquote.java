package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.cons.Append;
import core.scm.*;

import java.util.List;

import static core.scm.specialforms.Unquote.UNQUOTE;
import static core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING;

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
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
    return quasiquote(0, expression.get(1), env, evaluator);
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
  private Object quasiquote(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
    if (expr instanceof SCMVector) {
      SCMVector vector = (SCMVector) expr;
      if (vector.length() > 0 && (UNQUOTE.symbol().equals(vector.get(0)))) {
        throw new IllegalSyntaxException("unquote: invalid context within quasiquote");
      }
      if (vector.length() > 0 && (UNQUOTE_SPLICING.symbol().equals(vector.get(0)))) {
        throw new IllegalSyntaxException("unquote-splicing: invalid context within quasiquote");
      }
      return quasiquoteVector(level, expr, env, evaluator);
    } else if (expr instanceof List) {
      List list = (List) expr;
      if (list.size() > 0 && (UNQUOTE.symbol().equals(list.get(0)))) {
        if (list.size() != 2) {
          throw new IllegalSyntaxException("unquote: expects exactly one expression");
        }
        return evaluator.eval(list.get(1), env);
      }
      if (list.size() > 0 && (UNQUOTE_SPLICING.symbol().equals(list.get(0)))) {
        throw new IllegalSyntaxException("unquote-splicing: invalid context within quasiquote");
      }
      return quasiquoteList(level, expr, env, evaluator);
    }
    /* (quasiquote datum) => (quote datum) */
    return expr;
  }

  // TODO Optimize and simplify
  private Object quasiquoteList(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
    List list = (List)expr;
    SCMCons result = SCMCons.list();
    int n = 0;
    for (Object o : list) {
      n += 1;
      /* Append quoted forms recursively */
      if (!(o instanceof List) || (SCMCons.NIL.equals(o))) {
        /* Check special cases: `(1 unquote 2) => `(1 . 2) */
        if (n > 1 && UNQUOTE.symbol().equals(o)) {
          if (n == list.size() - 1) {
            return Append.append2(result, evaluator.eval(list.get(n), env));
          } else {
            throw new IllegalSyntaxException("unquote: expects exactly one expression");
          }
        }
        if (UNQUOTE_SPLICING.symbol().equals(o)) {
          throw new IllegalSyntaxException("unquote-splicing: invalid context within quasiquote");
        }
        result = (SCMCons) Append.append2(result, SCMCons.list(o));
      } else {
        List el = (List) o;
        Object op = el.get(0);
        if (QUASIQUOTE.symbol().equals(op)) {
          /* Increase level of quasiquotation */
          result = (SCMCons) Append.append2(result, SCMCons.list(quasiquoteList(level + 1, o, env, evaluator)));
        } else if (UNQUOTE.symbol().equals(op) || (UNQUOTE_SPLICING.symbol().equals(op))) {
          if (el.size() != 2) {
            throw new IllegalSyntaxException(String.format("%s: expects exactly one expression", op));
          }
          if (level == 0) {
            /* Level of quasiquotation is 0 - eval! */
            if (UNQUOTE.symbol().equals(op)) {
              /* Unquote */
              result = (SCMCons) Append.append2(result, SCMCons.list(evaluator.eval(el.get(1), env)));
            } else {
              /* Unquote-Splicing */
              result = (SCMCons) Append.append2(result, evaluator.eval(el.get(1), env));
            }
          } else {
            /* Decrease level of quasiquotation */
            result = (SCMCons) Append.append2(result, SCMCons.list(quasiquoteList(level - 1, o, env, evaluator)));
          }
        } else {
          result = (SCMCons) Append.append2(result, SCMCons.list(quasiquoteList(level, o, env, evaluator)));
        }
      }
    }
    return result;
  }

  // TODO Optimize vector->list and list-<vector conversions
  private Object quasiquoteVector(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
    SCMVector vector = (SCMVector)expr;
    Object result = quasiquoteList(level, SCMCons.list(vector.getArray()), env, evaluator);
    if (result instanceof List) {
      List list = (List) result;
      return new SCMVector(list.toArray());
    } else {
      return result;
    }
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
