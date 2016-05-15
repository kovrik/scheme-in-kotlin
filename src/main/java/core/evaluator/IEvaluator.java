package core.evaluator;

import core.environment.IEnvironment;

public interface IEvaluator {

  Object eval(Object sexp, IEnvironment env);
}
