package main.core.environment;

import main.core.ast.SCMSymbol;
import main.core.math.bool.Eq;
import main.core.math.bool.Equal;
import main.core.math.bool.Eqv;
import main.core.math.bool.Negation;
import main.core.math.numeric.*;
import main.core.specialforms.SpecialForm;

public final class DefaultEnvironment extends Environment {

  public DefaultEnvironment() {

    super(null);

    /* Special Forms */
    for (SpecialForm specialForm : SpecialForm.values()) {
      put(specialForm, specialForm);
    }

    /* Boolean */
    put(new SCMSymbol("#t"), true);
    put(new SCMSymbol("#f"), false);

    put(new SCMSymbol("not"), new Negation());

    /* nil */
    put(new SCMSymbol("#nil"), null);

    /* math */
    put(new SCMSymbol("+"), new Addition());
    put(new SCMSymbol("-"), new Subtraction());
    put(new SCMSymbol("*"), new Multiplication());
    put(new SCMSymbol("/"), new Division());

    /* Comparison & Equality */
    put(new SCMSymbol("="),  new NumericalComparison(NumericalComparison.Type.EQUAL));
    put(new SCMSymbol("<"),  new NumericalComparison(NumericalComparison.Type.LESS));
    put(new SCMSymbol("<="), new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    put(new SCMSymbol(">"),  new NumericalComparison(NumericalComparison.Type.GREATER));
    put(new SCMSymbol(">="), new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));

    put(new SCMSymbol("eq?"),    new Eq());
    put(new SCMSymbol("eqv?"),   new Eqv());
    put(new SCMSymbol("equal?"), new Equal());
  }
}
