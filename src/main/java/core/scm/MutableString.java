package core.scm;

/**
 * Mutable Scheme String
 *
 * Java String is immutable, StringBuilder is mutable.
 * But both classes are final and cannot be extended.
 * This MutableString class holds a mutable StringBuilder instance
 * and delegates it all string operations.
 */
public class MutableString implements INamed, CharSequence {

  private final StringBuilder string;

  public MutableString() {
    this.string = new StringBuilder();
  }

  public MutableString(String string) {
    this.string = new StringBuilder(string);
  }

  public MutableString(int length) {
    this.string = new StringBuilder(length);
  }

  public MutableString append(Object c) {
    this.string.append(c);
    return this;
  }

  public int length() {
    return this.string.length();
  }

  @Override
  public char charAt(int index) {
    return string.charAt(index);
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    return string.subSequence(start, end);
  }

  public void setCharAt(int index, char ch) {
    this.string.setCharAt(index, ch);
  }

  public void setLength(int n) {
    string.setLength(n);
  }

  public void clear() {
    string.setLength(0);
  }

  public MutableString reverse() {
    string.reverse();
    return this;
  }

  @Override
  public String getName() {
    return string.toString();
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
    if (!(o instanceof CharSequence)) {
      return false;
    }
    return string.toString().equals(o.toString());
  }

  @Override
  public int hashCode() {
    return string.toString().hashCode();
  }
}
