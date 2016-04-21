package main.environment;

import main.ast.SCMSymbol;
import main.core.math.bool.Eq;
import main.core.math.bool.Equal;
import main.core.math.bool.Eqv;
import main.core.math.bool.Negation;
import main.core.math.numeric.*;
import main.core.specialforms.SpecialForm;

public final class DefaultEnvironment {

  private DefaultEnvironment() {
    // default
  }

  private static final Environment ENV = new Environment(null);
  static {
    /* Special Forms */
    for (SpecialForm specialForm : SpecialForm.values()) {
      ENV.put(specialForm, specialForm);
    }

    /* Boolean */
    ENV.put(new SCMSymbol("#t"), true);
    ENV.put(new SCMSymbol("#f"), false);

    ENV.put(new SCMSymbol("not"), new Negation());

    /* nil */
    ENV.put(new SCMSymbol("#nil"), null);

    /* math */
    ENV.put(new SCMSymbol("+"), new Addition());
    ENV.put(new SCMSymbol("-"), new Subtraction());
    ENV.put(new SCMSymbol("*"), new Multiplication());
    ENV.put(new SCMSymbol("/"), new Division());

    /* Comparison & Equality */
    ENV.put(new SCMSymbol("="),  new NumericalComparison(NumericalComparison.Type.EQUAL));
    ENV.put(new SCMSymbol("<"),  new NumericalComparison(NumericalComparison.Type.LESS));
    ENV.put(new SCMSymbol("<="), new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    ENV.put(new SCMSymbol(">"),  new NumericalComparison(NumericalComparison.Type.GREATER));
    ENV.put(new SCMSymbol(">="), new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));

    ENV.put(new SCMSymbol("eq?"),    new Eq());
    ENV.put(new SCMSymbol("eqv?"),   new Eqv());
    ENV.put(new SCMSymbol("equal?"), new Equal());
  }

  public static Environment getEnv() {
    return ENV;
  }
}
