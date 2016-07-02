package core.scm;

public class SCMCons implements IPair{

  // TODO: Optimize
  // TODO: Implement SCMLists using SCMCons?
  private Object car;
  private Object cdr;

  private SCMCons(Object car, Object cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  public Object car() {
    return car;
  }

  public Object cdr() {
    return cdr;
  }

  public boolean isPair() {
    return true;
  }

  public boolean isNull() {
    return false;
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

  // TODO IPair interface?
  public static boolean isPair(Object o) {
    return ((o instanceof SCMCons) || (o instanceof SCMList)) && (!SCMList.NIL.equals(o));
  }
}
