package main.core;

import main.environment.Environment;

public interface IEvaluator {

  Object eval(Object sexp, Environment env);
}
