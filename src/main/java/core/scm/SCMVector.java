package core.scm;

import java.util.Arrays;

/* Mutable vector */
public class SCMVector implements ISCMClass {

  private final Object[] vector;

  public SCMVector() {
    this.vector = new Object[0];
  }

  public SCMVector(int size) {
    this.vector = new Object[size];
  }

  public SCMVector(int size, Object init) {
    this.vector = new Object[size];
    Arrays.fill(vector, init);
  }

  public SCMVector(Object... elements) {
    this.vector = elements;
  }

  public SCMVector(Object e) {
    this.vector = new Object[] {e};
  }

  public Object get(int index) {
    return vector[index];
  }

  public void set(int index, Object value) {
    vector[index] = value;
  }

  public int length() {
    return vector.length;
  }

  public Object[] getArray() {
    return vector;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.VECTOR;
  }

  @Override
  public String toString() {
    if (vector.length == 0) {
      return "#()";
    }
    StringBuilder sb = new StringBuilder();
    sb.append("#(");
    for (int i = 0; i < vector.length; i++) {
      Object e = vector[i];
      if (e == this) {
        sb.append("(this Vector)");
      } else if (e instanceof String || e instanceof SCMString) {
        sb.append('"').append(e).append('"');
      } else {
        sb.append(e);
      }
      if (i == (vector.length - 1)) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
    return sb.toString();
  }

  public Object first() {
    if (vector.length == 0) {
      throw new IllegalArgumentException("Value out of range");
    }
    return vector[0];
  }

  public SCMVector rest() {
    if (vector.length < 2) {
      return new SCMVector();
    }
    return new SCMVector(Arrays.copyOfRange(vector, 1, vector.length));
  }

  @Override
  public int hashCode() {
    return super.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof SCMVector && Arrays.equals(vector, ((SCMVector) obj).vector);
  }
}
