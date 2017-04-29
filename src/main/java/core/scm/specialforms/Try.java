package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.evaluator.Reflector;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ThrowableWrapper;
import core.scm.SCMCons;
import core.scm.SCMNil;
import core.scm.SCMSymbol;

import java.util.*;

public enum Try implements ISpecialForm {
  TRY;

  private static final Reflector REFLECTOR = new Reflector();

  private static final SCMSymbol CATCH   = SCMSymbol.intern("catch");
  private static final SCMSymbol FINALLY = SCMSymbol.intern("finally");

  @Override
  public String toString() {
    return "try";
  }

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.isEmpty()) {
      return null;
    }
    boolean hadCatch = false;
    Map<Class, Object> catches = Collections.emptyMap();
    Map<Class, SCMSymbol> catchBindings = Collections.emptyMap();
    Object fin = null;
    List<Object> expressions = new ArrayList<>();
    /* Init and check syntax */
    for (int i = 1; i < expression.size(); i++) {
      Object e = expression.get(i);
      if ((e instanceof List) && !((List) e).isEmpty()) {
        List expr = (List)e;
        Object op = expr.get(0);
        if (FINALLY.equals(op)) {
          if (i != expression.size() - 1) {
            throw new IllegalSyntaxException("try: finally clause must be last in try expression");
          }
          if (expr.size() > 1) {
            fin = SCMCons.list(Begin.BEGIN);
            ((SCMCons)fin).addAll(expr.subList(1, expr.size()));
          }
          continue;
        } else if (CATCH.equals(op)) {
          if (expr.size() < 3) {
            throw new IllegalSyntaxException("catch: bad syntax in form: " + expr);
          }
          hadCatch = true;
          if (catches.isEmpty()) {
            catches = new HashMap<>();
            catchBindings = new HashMap<>();
          }
          Class clazz = REFLECTOR.getClazz(expr.get(1).toString());
          Object catchExpr = SCMNil.NIL;
          if (expr.size() > 3) {
            catchExpr = SCMCons.list(Begin.BEGIN);
            ((SCMCons)catchExpr).addAll(expr.subList(3, expr.size()));
          }
          catches.put(clazz, catchExpr);
          Object cb = expr.get(2);
          if (!(cb instanceof SCMSymbol)) {
            throw new IllegalSyntaxException("catch: bad binding form, expected Symbol, actual: " + cb);
          }
          catchBindings.put(clazz, (SCMSymbol) cb);
          continue;
        } else {
          if (hadCatch) {
            throw new IllegalSyntaxException("try: only catch or finally clause can follow catch in try expression");
          }
        }
      }
      expressions.add(e);
    }
    /* Now Evaluate everything */
    try {
      Object result = null;
      for (Object e : expressions) {
        result = evaluator.eval(e, env);
      }
      return result;
    } catch (Throwable e) {
      /* Unwrap if it is a ThrowableWrapper */
      e = (e instanceof ThrowableWrapper) ? e.getCause() : e;
      /* Check if we had catch block for that type of exception */
      Object catchBlock = catches.get(e.getClass());
      if (catchBlock != null) {
        /* Bind exception */
        env.put(catchBindings.get(e.getClass()), e);
        /* Evaluate corresponding catch block */
        return evaluator.eval(catchBlock, env);
      }
      /* Unexpected exception, re-throw it */
      throw new ThrowableWrapper(e);
    } finally {
      /* And finally, evaluate finally block (if present) */
      if (fin != null) {
        evaluator.eval(fin, env);
      }
    }
  }
}
