package core.scm;

public class SCMSymbol {

  private final String value;

  public SCMSymbol(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof SCMSymbol)) return false;
    SCMSymbol scmSymbol = (SCMSymbol) o;
    return value != null ? value.equals(scmSymbol.value) : scmSymbol.value == null;
  }

  @Override
  public int hashCode() {
    return value != null ? value.hashCode() : 0;
  }

  @Override
  public String toString() {
    return value;
  }
}
