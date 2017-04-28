package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.evaluator.Reflector;
import core.evaluator.ReflectorResult;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;

import java.util.List;

public enum Dot implements ISpecialForm {
  DOT;

  private final Reflector reflector = new Reflector();

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    int size = expression.size();
    if (size < 3) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", size - 1));
    }
    // FIXME Optimize and cleanup
    Object first = expression.get(1);
    if (first instanceof SCMSymbol) {
      first = env.findOrDefault(first, null);
      if (first == null) {
        first = evaluator.eval(expression.get(1), env);
      }
    } else {
      first = evaluator.eval(first, env);
    }
    Object result;
    if (first instanceof Class) {
      String statik = expression.get(1) + "/" + expression.get(2);
      if (expression.size() == 3) {
        /* (. Classname-symbol member-symbol) */
        /* try static field first */
        try {
          result = reflector.evalJavaStaticField(statik);
        } catch (RuntimeException e) {
          if (e.getCause() instanceof NoSuchFieldException) {
            /* now try static no-args static method */
            result = reflector.evalJavaMethod(statik, new Object[]{});
          } else {
            throw e;
          }
        }
      } else {
        /* (. Classname-symbol method-symbol args) */
        Object[] args = new Object[expression.size() - 3];
        /* Add args */
        for (int i = 0; i < args.length; i++) {
          args[i] = evaluator.eval(expression.get(i + 3), env);
        }
        result = reflector.evalJavaMethod(statik, args);
      }
    } else {
      /* (. instance-expr member-symbol)
       * (. instance-expr -field-symbol)
       * (. instance-expr method-symbol args)
       */
      String method = '.' + expression.get(2).toString();
      Object[] args = new Object[expression.size() - 2];
      /* Add instance */
      args[0] = first;
      /* Add rest args (if any) */
      for (int i = 1; i < args.length; i++) {
        args[i] = evaluator.eval(expression.get(i + 2), env);
      }
      result = reflector.evalJavaMethod(method, args);
    }
    return ReflectorResult.maybeWrap(result);
  }

  @Override
  public String toString() {
    return ".";
  }
}
