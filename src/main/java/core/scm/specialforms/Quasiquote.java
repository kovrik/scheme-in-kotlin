package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.cons.Append;
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
    return quasiquote(0, expression.get(1), env, evaluator);
  }

  private Object quasiquote(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
    if (expr instanceof SCMVector) {
      return quasiquoteVector(level, expr, env, evaluator);
    } else if (expr instanceof List) {
      List list = (List) expr;
      if (list.size() > 0 && (Unquote.UNQUOTE.symbol().equals(list.get(0)))) {
        if (list.size() != 2) {
          throw new IllegalSyntaxException("unquote: expects exactly one expression");
        }
        return evaluator.eval(list.get(1), env);
      }
      if (list.size() > 0 && (UnquoteSplicing.UNQUOTE_SPLICING.symbol().equals(list.get(0)))) {
        throw new IllegalSyntaxException("unquote-splicing: invalid context within quasiquote");
      }
      return quasiquoteList(level, expr, env, evaluator);
    }
    /* (quasiquote datum) => (quote datum) */
    return expr;
  }

  private Object quasiquoteList(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
    List list = (List)expr;
    SCMCons result = SCMCons.list();
    for (Object o : list) {
      /* Append quoted forms recursively */
      if (!(o instanceof List) || (SCMCons.NIL.equals(o))) {
        result = (SCMCons) Append.append2(result, SCMCons.list(o));
      } else {
        List el = (List) o;
        Object op = el.get(0);
        if (QUASIQUOTE.symbol().equals(op)) {
          /* Increase level of quasiquotation */
          result = (SCMCons) Append.append2(result, SCMCons.list(quasiquoteList(level + 1, o, env, evaluator)));
        } else if (Unquote.UNQUOTE.symbol().equals(op) || (UnquoteSplicing.UNQUOTE_SPLICING.symbol().equals(op))) {
          if (el.size() != 2) {
            throw new IllegalSyntaxException(String.format("%s: expects exactly one expression", op));
          }
          if (level == 0) {
            /* Level of quasiquotation is 0 - eval! */
            if (Unquote.UNQUOTE.symbol().equals(op)) {
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

//  private Object quasiquoteList(int level, Object expr, IEnvironment env, IEvaluator evaluator) {
//
//    List list = (List)expr;
//    /* (quasiquote datum) => (quote datum) */
//    if (list.isEmpty()) {
//      return list;
//    }
//    /* (quasiquote (unquote datum)) => datum */
//    Object o = list.get(0);
//    /* unquote */
//    if ((o instanceof SCMSymbol) && ("unquote".equals(((SCMSymbol) o).getValue()))) {
//      if (list.size() != 2) {
//        throw new IllegalSyntaxException("unquote: expects exactly one expression");
//      }
//      if (level == 0) {
//        return evaluator.eval(list.get(1), env);
//      } else {
//        SCMCons<Object> result = SCMCons.list();
//        result.add(o);
//        result.add(quasiquote(level - 1, list.get(1), env, evaluator));
//        return result;
//      }
//    }
//
//    if ((o instanceof SCMSymbol) && ("quasiquote".equals(((SCMSymbol) o).getValue()))) {
//      level += 1;
//    }
//    // TODO unquote-splicing
//
//    Object car = Car.car(list);
//    Object carResult = quasiquote(level, car, env, evaluator);
//
//    Object cdr = Cdr.cdr(list);
//    Object cdrResult = quasiquote(level, cdr, env, evaluator);
//
//    return SCMCons.cons(carResult, cdrResult);
//  }

  private Object quasiquoteVector(int level, Object expr, IEnvironment env, IEvaluator evaluator) {

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
      if (level == 0) {
        return evaluator.eval(vector.get(1), env);
      } else {
        SCMVector result = new SCMVector(2);
        result.set(0, o);
        result.set(1, quasiquote(level - 1, vector.get(1), env, evaluator));
        return result;
      }
    }

    if ((o instanceof SCMSymbol) && ("quasiquote".equals(((SCMSymbol) o).getValue()))) {
      level += 1;
    }
    // TODO Nested quasiquotation
    // TODO unquote-splicing

    /* (quasiquote #(first . rest)) => #((quasiquote first) (quasiquote rest)) */
    Object car = quasiquote(level, vector.get(0), env, evaluator);
    SCMVector cdr = (SCMVector)quasiquote(level, vector.rest(), env, evaluator);

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
