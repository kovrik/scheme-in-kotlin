package main.core.evaluator;

import main.core.environment.IEnvironment;

public interface IEvaluator {

  Object eval(Object sexp, IEnvironment env);
}
