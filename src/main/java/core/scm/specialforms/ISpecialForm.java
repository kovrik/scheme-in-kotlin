package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;

import java.util.List;

public interface ISpecialForm extends ISCMClass {

  Object eval(List<Object> expression, Environment env, Evaluator evaluator);

  @Override
  default SCMClass getSCMClass() {
    return SCMClass.SPECIAL_FORM;
  }
}
