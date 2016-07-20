package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;

import java.util.List;

public interface ISpecialForm {

  Object eval(List expression, IEnvironment env, IEvaluator evaluator);
}
