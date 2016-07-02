package core.scm;

public class SCMCons {

  // TODO: Optimize
  // TODO: Implement SCMLists using SCMCons?
  private final Object car;
  private final Object cdr;

  private SCMCons(Object car, Object cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  @Override
  public String toString() {

    if (cdr == null) {
      return "(" + car + ")";
    }
    if (cdr instanceof SCMCons) {
      // TODO Get rid of recursion
      String s = cdr.toString();
      return "(" + car + " " + s.substring(1, s.length() - 1) + ")";
    }
    return "(" + car + " . " + cdr + ")";
  }

  public static SCMCons cons(Object car, Object cdr) {
    return new SCMCons(car, cdr);
  }
}
