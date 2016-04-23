package main.core.evaluator;

import main.environment.IEnvironment;

public interface IEvaluator {

  Object eval(Object sexp, IEnvironment env);
}
