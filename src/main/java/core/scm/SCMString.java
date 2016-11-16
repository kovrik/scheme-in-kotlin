package core.scm;

public class SCMString implements ISCMClass {

  private StringBuilder string;

  public SCMString() {
    this.string = new StringBuilder();
  }

  public SCMString(String string) {
    this.string = new StringBuilder(string);
  }

  public SCMString(int length) {
    this.string = new StringBuilder(length);
  }

  public SCMString append(Object c) {
    this.string.append(c);
    return this;
  }

  public int length() {
    return this.string.length();
  }

  public void setCharAt(int index, char ch) {
    this.string.setCharAt(index, ch);
  }

  public String substring(int start) {
    return this.string.substring(start);
  }

  public String substring(int start, int end) {
    return this.string.substring(start, end);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.STRING;
  }

  @Override
  public String toString() {
    return string.toString();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || !(o instanceof SCMString || o instanceof String)) {
      return false;
    }
    return toString().equals(o.toString());
  }

  @Override
  public int hashCode() {
    return string != null ? string.hashCode() : 0;
  }
}
