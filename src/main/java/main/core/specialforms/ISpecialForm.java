package main.core.specialforms;

import main.core.ast.SCMList;
import main.core.evaluator.IEvaluator;
import main.core.environment.IEnvironment;

public interface ISpecialForm {

  Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator);
}
