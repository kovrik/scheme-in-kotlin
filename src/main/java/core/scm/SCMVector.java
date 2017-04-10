package core.scm;

import core.writer.Writer;

import java.util.Arrays;
import java.util.Iterator;

/* Abstract superclass of both Immutable and Mutable Vectors */
public abstract class SCMVector implements ISCMClass, Iterable {

  /* Scheme Vector syntax */
//  private static final String OPEN = "#(";
//  private static final String CLOSE = ")";
  /* Alternative (Clojure-like) Vector syntax */
  private static final String OPEN = "[";
  private static final String CLOSE = "]";

  // TODO Replace with ArrayList?
  /* Contents of Vector: plain Java array */
  private final Object[] array;

  SCMVector() {
    this.array = new Object[0];
  }

  SCMVector(int size, Object init) {
    this.array = new Object[size];
    Arrays.fill(getArray(), init);
  }

  SCMVector(Object... elements) {
    this.array = elements;
  }

  SCMVector(Object e) {
    this.array = new Object[] {e};
  }

  public Object get(int index) {
    return getArray()[index];
  }

  public int length() {
    return getArray().length;
  }

  public Object[] getArray() {
    return array;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.VECTOR;
  }

  @Override
  public String toString() {
    if (getArray().length == 0) {
      return OPEN + CLOSE;
    }
    StringBuilder sb = new StringBuilder();
    sb.append(OPEN);
    for (int i = 0; i < getArray().length; i++) {
      Object e = getArray()[i];
      if (e == this) {
        sb.append("(this Vector)");
      } else {
        sb.append(Writer.write(e));
      }
      if (i == (getArray().length - 1)) {
        return sb.append(CLOSE).toString();
      }
      sb.append(' ');
    }
    return sb.toString();
  }

  @Override
  public Iterator<Object> iterator() {
    return Arrays.asList(array).iterator();
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(getArray());
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof SCMVector && Arrays.equals(getArray(), ((SCMVector) obj).getArray());
  }
}
