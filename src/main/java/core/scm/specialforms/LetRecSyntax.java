package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;

import java.util.List;

// TODO
public enum LetRecSyntax implements ISpecialForm {
  LETREC_SYNTAX;

  private static final String syntax = "letrec-syntax";

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    throw new UnsupportedOperationException("NOT IMPLEMENTED YET!");
  }

  @Override
  public String toString() {
    return syntax;
  }
}
