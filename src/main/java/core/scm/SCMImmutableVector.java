package core.scm;

import java.util.Arrays;

/* Immutable vector */
public class SCMImmutableVector extends SCMVector {

  private final Object[] vector;

  public SCMImmutableVector(Object... elements) {
    this.vector = elements;
  }

  @Override
  public Object get(int index) {
    return vector[index];
  }

  @Override
  public int length() {
    return vector.length;
  }

  @Override
  public Object[] getArray() {
    return vector;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.IMMUTABLE_VECTOR;
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
      } else if (e instanceof String || e instanceof SCMMutableString) {
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

  @Override
  public int hashCode() {
    return Arrays.hashCode(vector);
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof SCMVector && Arrays.equals(vector, ((SCMVector) obj).getArray());
  }
}
