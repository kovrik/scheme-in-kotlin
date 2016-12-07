package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;

import java.util.List;

// TODO
public enum LetSyntax implements ISpecialForm {
  LET_SYNTAX;

  private static final String syntax = "let-syntax";

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    throw new UnsupportedOperationException("NOT IMPLEMENTED YET!");
  }

  @Override
  public String toString() {
    return syntax;
  }
}
