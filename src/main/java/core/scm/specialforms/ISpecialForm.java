package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;

import java.util.List;

public interface ISpecialForm {

  Object eval(List<Object> expression, Environment env, Evaluator evaluator);
}
