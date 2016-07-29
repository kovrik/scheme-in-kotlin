package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

// TODO
public class SyntaxRules implements ISpecialForm, ISCMClass {

  public static final SyntaxRules SYNTAX_RULES = new SyntaxRules();

  private final String syntax = "syntax-rules";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private SyntaxRules() {}

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
