package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;
import java.util.function.BiPredicate;

public final class NumericalComparison extends AFn {

  public static final NumericalComparison EQUAL         = new NumericalComparison("=",  (f, s) -> f.compareTo(s) == 0);
  public static final NumericalComparison LESS          = new NumericalComparison("<",  (f, s) -> f.compareTo(s) <  0);
  public static final NumericalComparison GREATER       = new NumericalComparison(">",  (f, s) -> f.compareTo(s) >  0);
  public static final NumericalComparison LESS_EQUAL    = new NumericalComparison("<=", (f, s) -> f.compareTo(s) <= 0);
  public static final NumericalComparison GREATER_EQUAL = new NumericalComparison(">=", (f, s) -> f.compareTo(s) >= 0);

  private final String name;
  private final BiPredicate<Comparable<Number>, Number> predicate;

  private NumericalComparison(String name, BiPredicate<Comparable<Number>, Number> predicate) {
    super(new FnArgsBuilder().minArgs(2)
                             .mandatoryArgsTypes(new Class[]{SCMClass.Real.class, SCMClass.Real.class})
                             .restArgsType(SCMClass.Real.class));
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Boolean apply(Object... args) {
    for (int i = 0; i < args.length - 1; i++) {
      Number f = (Number)args[i];
      Number s = (Number)args[i + 1];
      if (f instanceof SCMBigRational) {
        f = ((SCMBigRational)f).toBigDecimal();
      }
      if (s instanceof SCMBigRational) {
        s = ((SCMBigRational)s).toBigDecimal();
      }
      if ((f instanceof Double) || (s instanceof Double)) {
        f = f.doubleValue();
        s = s.doubleValue();
      } else if ((f instanceof BigDecimal) || (s instanceof BigDecimal)) {
        f = new BigDecimal(f.toString());
        s = new BigDecimal(s.toString());
      }
      if (!predicate.test((Comparable)f, s)) {
        return Boolean.FALSE;
      }
    }
    return Boolean.TRUE;
  }
}
