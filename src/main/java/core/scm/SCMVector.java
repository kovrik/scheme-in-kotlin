package core.scm;

import java.util.Arrays;

/* Abstract superclass of both Immutable and Mutable Vectors */
public abstract class SCMVector implements ISCMClass {

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
      return "#()";
    }
    StringBuilder sb = new StringBuilder();
    sb.append("#(");
    for (int i = 0; i < getArray().length; i++) {
      Object e = getArray()[i];
      if (e == this) {
        sb.append("(this Vector)");
      } else if (e instanceof String || e instanceof SCMMutableString) {
        sb.append('"').append(e).append('"');
      } else {
        sb.append(e);
      }
      if (i == (getArray().length - 1)) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
    return sb.toString();
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
