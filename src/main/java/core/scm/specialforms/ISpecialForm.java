package core.scm.specialforms;

import core.scm.SCMList;
import core.evaluator.IEvaluator;
import core.environment.IEnvironment;

public interface ISpecialForm {

  Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator);
}
