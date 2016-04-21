package main.core.specialforms;

import main.ast.SCMList;
import main.core.IEvaluator;
import main.environment.Environment;

public interface ISpecialForm {

  Object eval(SCMList<Object> expression, Environment env, IEvaluator evaluator);
}
