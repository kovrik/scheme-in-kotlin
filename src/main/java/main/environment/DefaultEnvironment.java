package main.environment;

import main.expressions.Keywords;
import main.functions.equality.*;
import main.functions.math.*;

public class DefaultEnvironment {

  private static final Environment ENV = new Environment(null);
  static {
    // boolean
//    for (SCMBoolean scmBoolean : SCMBoolean.values()) {
//      ENV.put(scmBoolean.getValue(), scmBoolean.getResult());
//    }
    ENV.put("#t", true);
    ENV.put("#f", false);

    ENV.put(Keywords.AND, new Conjunction());
    ENV.put(Keywords.OR,  new Disjunction());
    ENV.put(Keywords.NOT, new Negation());

    // nil
    ENV.put("#nil", null);

    // math
    ENV.put("+", new Addition());
    ENV.put("-", new Subtraction());
    ENV.put("*", new Multiplication());
    ENV.put("/", new Division());

    // equality
    ENV.put(Keywords.NUM_EQUALITY, new NumericalComparison(NumericalComparison.Type.EQUAL));
    ENV.put(Keywords.LESS, new NumericalComparison(NumericalComparison.Type.LESS));
    ENV.put(Keywords.LESS_EQUAL, new NumericalComparison(NumericalComparison.Type.LESS_EQUAL));
    ENV.put(Keywords.GREATER, new NumericalComparison(NumericalComparison.Type.GREATER));
    ENV.put(Keywords.GREATER_EQUAL, new NumericalComparison(NumericalComparison.Type.GREATER_EQUAL));
    ENV.put(Keywords.EQ, new Eq());
    ENV.put(Keywords.EQV, new Eqv());
    ENV.put(Keywords.EQUAL, new Equal());

//    ENV.put(Keywords.COND, new Cond());
  }

  public static Environment getEnv() {
    return ENV;
  }
}
