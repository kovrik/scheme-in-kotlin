package core.scm;

public class SCMPair<F, S> {

  private F car;
  private S cdr;

  public SCMPair(F car, S cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    SCMPair<?, ?> scmPair = (SCMPair<?, ?>) o;

    if (car != null ? !car.equals(scmPair.car) : scmPair.car != null) return false;
    return cdr != null ? cdr.equals(scmPair.cdr) : scmPair.cdr == null;
  }

  @Override
  public int hashCode() {
    int result = car != null ? car.hashCode() : 0;
    result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
    return result;
  }

  @Override
  public String toString() {
    return String.format("(%s . %s)", car, cdr);
  }
}
