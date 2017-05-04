package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.ITyped;
import core.scm.Type;

import java.util.List;

public interface ISpecialForm extends ITyped {

  Object eval(List<Object> expression, Environment env, Evaluator evaluator);

  @Override
  default Type getType() {
    return Type.SPECIAL_FORM;
  }
}
