package core.environment;

import core.procedures.delayed.Force;
import core.procedures.delayed.Promise;
import core.procedures.equivalence.*;
import core.procedures.math.bool.Negation;
import core.procedures.math.numeric.*;
import core.scm.SCMBoolean;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;

import java.util.HashMap;
import java.util.Map;

public final class DefaultEnvironment extends Environment {

  private final Map<String, String> procs = new HashMap<String, String>();
  {
    procs.put("promise?", String.format("(define (promise? o) (string=? \"%s\" (class-of o)))", Promise.class.getName()));
    procs.put("char?",    String.format("(define (char? o)    (string=? \"%s\" (class-of o)))", Character.class.getName()));
    procs.put("string?",  String.format("(define (string? o)  (string=? \"%s\" (class-of o)))", String.class.getName()));
    procs.put("vector?",  String.format("(define (vector? o)  (string=? \"%s\" (class-of o)))", SCMVector.class.getName()));
  }

  public Map<String, String> getProcs() {
    return procs;
  }

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

//    put(new SCMSymbol("string?"), new IsAString());
    put(new SCMSymbol("string=?"), new StringEq());
    put(new SCMSymbol("string-ci=?"), new StringEqCi());

//    put(new SCMSymbol("char?"), new IsAChar());
    put(new SCMSymbol("char=?"), new CharEq());
    put(new SCMSymbol("char-ci=?"), new CharEqCi());

//    put(new SCMSymbol("promise?"), new IsAPromise());

    put(new SCMSymbol("force"), new Force());

    /* Vectors */
//    put(new SCMSymbol("vector?"), new IsAVector());
  }
}
