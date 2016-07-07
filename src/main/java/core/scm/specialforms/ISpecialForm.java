package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;

public interface ISpecialForm {

  Object eval(SCMCons expression, IEnvironment env, IEvaluator evaluator);
}
