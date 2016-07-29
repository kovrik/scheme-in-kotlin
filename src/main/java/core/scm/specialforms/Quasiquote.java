package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

// TODO
public class Quasiquote implements ISpecialForm, ISCMClass {

  public static final Quasiquote QUASIQUOTE = new Quasiquote();

  private final String syntax = "quasiquote";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Quasiquote() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    throw new UnsupportedOperationException("NOT IMPLEMENTED YET!");
  }

  public SCMSymbol symbol() {
    return symbol;
  }

  @Override
  public String toString() {
    return syntax;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
