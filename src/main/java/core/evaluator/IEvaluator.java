package core.evaluator;

import core.environment.IEnvironment;

public interface IEvaluator {

  /* Eval with trampoline */
  Object eval(Object sexp, IEnvironment env);

  Object evalp(Object sexp, IEnvironment env);
}
