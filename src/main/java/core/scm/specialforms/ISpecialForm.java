package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.List;

public interface ISpecialForm extends ISCMClass {

  Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator);

  @Override
  default SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
