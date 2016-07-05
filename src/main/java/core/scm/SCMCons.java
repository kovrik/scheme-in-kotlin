package core.scm;

public class SCMCons implements IPair {

  private Object car;
  private Object cdr;

  public SCMCons(Object car, Object cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  public Object cons(Object a) {
    return new SCMCons(a, this);
  }

  public Object car() {
    return car;
  }

  public Object cdr() {
    return cdr;
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
}
