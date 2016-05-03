package main.core.environment;

import main.core.ast.SCMSymbol;
import main.core.evaluator.IEvaluator;
import main.core.math.bool.Eq;
import main.core.math.bool.Equal;
import main.core.math.bool.Eqv;
import main.core.math.bool.Negation;
import main.core.math.numeric.*;
import main.core.specialforms.SpecialForm;

public final class DefaultEnvironment extends Environment {

  public DefaultEnvironment(IEvaluator evaluator) {

    super(null);

    /* Special Forms */
    for (SpecialForm specialForm : SpecialForm.values()) {
      this.put(specialForm, specialForm);
    }

    /* Boolean */
    this.put(new SCMSymbol("#t"), true);
    this.put(new SCMSymbol("#f"), false);

    this.put(new SCMSymbol("not"), new Negation());

    /* nil */
    this.put(new SCMSymbol("#nil"), null);

    /* math */
    this.put(new SCMSymbol("+"), new Addition());
    this.put(new SCMSymbol("-"), new Subtraction());
    this.put(new SCMSymbol("*"), new Multiplication());
    this.put(new SCMSymbol("/"), new Division());

    /* Comparison & Equality */
    this.put(new SCMSymbol("="),  new NumericalComparison(NumericalComparison.Type.EQUAL));
    this.put(new SCMSymbol("<"),  new NumericalComparison(NumericalComparison.Type.LESS));
    this.put(new SCMSymbol("<="), new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    this.put(new SCMSymbol(">"),  new NumericalComparison(NumericalComparison.Type.GREATER));
    this.put(new SCMSymbol(">="), new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));

    this.put(new SCMSymbol("eq?"),    new Eq());
    this.put(new SCMSymbol("eqv?"),   new Eqv());
    this.put(new SCMSymbol("equal?"), new Equal());
  }
}
