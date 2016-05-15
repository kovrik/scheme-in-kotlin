package core.environment;

import core.procedures.characters.IsAChar;
import core.procedures.delayed.Force;
import core.procedures.equivalence.*;
import core.procedures.math.bool.Negation;
import core.procedures.math.numeric.*;
import core.procedures.strings.IsAString;
import core.procedures.vectors.IsAVector;
import core.procedures.vectors.MakeVector;
import core.scm.SCMBoolean;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

public final class DefaultEnvironment extends Environment {

  public DefaultEnvironment() {

    super(null);

    /* Special Forms */
    for (SCMSpecialForm specialForm : SCMSpecialForm.values()) {
      put(specialForm, specialForm);
    }

    /* Boolean */
    put(SCMBoolean.TRUE,  SCMBoolean.TRUE);
    put(SCMBoolean.FALSE, SCMBoolean.FALSE);

    put(new SCMSymbol("not"), new Negation());

    /* nil */
    // TODO?
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

    put(new SCMSymbol("string?"), new IsAString());
    put(new SCMSymbol("string=?"), new StringEq());
    put(new SCMSymbol("string-ci=?"), new StringEqCi());

    put(new SCMSymbol("char?"), new IsAChar());
    put(new SCMSymbol("char=?"), new CharEq());
    put(new SCMSymbol("char-ci=?"), new CharEqCi());

//    put(new SCMSymbol("promise?"), new IsAPromise());

    put(new SCMSymbol("force"), new Force());

    /* Vectors */
    put(new SCMSymbol("vector?"), new IsAVector());

    put(new SCMSymbol("make-vector"), new MakeVector());
  }
}
