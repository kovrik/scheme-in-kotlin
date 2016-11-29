package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

public class UnquoteSplicing implements ISpecialForm, ISCMClass {

  public static final UnquoteSplicing UNQUOTE_SPLICING = new UnquoteSplicing();

  private final String syntax = "unquote-splicing";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private UnquoteSplicing() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.of(syntax, expression, "not in quasiquote");
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
