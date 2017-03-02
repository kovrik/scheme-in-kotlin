package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMThunk;

import java.util.List;
import java.util.stream.IntStream;

/* Syntax:
 * (let <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
public enum Let implements ISpecialForm {
  LET;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    /* Normal let:
     * (let ((id expr) ...) body ...+) */
    if (expression.get(1) instanceof List) {
      Environment localEnv = new Environment(env);
      /* Evaluate inits */
      List<List<Object>> bindings = (List)expression.get(1);
      bindings.forEach(b -> {
        Object var = b.get(0);
        if (localEnv.get(var) != null) {
          throw IllegalSyntaxException.of(toString(), expression, String.format("duplicate identifier: %s", var));
        }
        Object init = b.get(1);
        localEnv.put(var, evaluator.eval(init, env));
      });
      /* Evaluate body */
      IntStream.range(2, expression.size() - 1).forEach(i -> evaluator.eval(expression.get(i), localEnv));
      return new SCMThunk(expression.get(expression.size() - 1), localEnv);

    } else if (expression.get(1) instanceof SCMSymbol) {
      // TODO Optimize and cleanup
      /* Named let:
       * (let proc-id ((arg-id init-expr) ...) body ...+) */
      Object o = expression.get(1);
      if (!(o instanceof SCMSymbol)) {
        throw IllegalSyntaxException.of(toString(), expression);
      }
      /* Construct lambda */
      SCMCons<Object> lambdaArgs = SCMCons.list();
      SCMCons<Object> initValues = SCMCons.list();
      List<List<Object>> bindings = (List)expression.get(2);
      bindings.forEach(b -> {
        Object arg = b.get(0);
        if (lambdaArgs.contains(arg)) {
          throw IllegalSyntaxException.of(toString(), expression, String.format("duplicate identifier: %s", arg));
        }
        lambdaArgs.add(arg);
        initValues.add(b.get(1));
      });

      Object lambdaBody = expression.get(3);
      SCMCons lambda = SCMCons.list(Lambda.LAMBDA, lambdaArgs, lambdaBody);
      SCMSymbol name = (SCMSymbol)o;
      SCMCons<SCMCons> l = SCMCons.list();
      l.add(SCMCons.list(name, lambda));

      SCMCons<Object> body = SCMCons.list(name);
      body.addAll(initValues);

      /* Named let is implemented via letrec */
      SCMCons<Object> letrec = SCMCons.list(LetRec.LETREC, l, body);
      /* Letrec has TCO */
      return LetRec.LETREC.eval(letrec, new Environment(env), evaluator);
    }
    throw IllegalSyntaxException.of(toString(), expression);
  }

  @Override
  public String toString() {
    return "let";
  }
}
