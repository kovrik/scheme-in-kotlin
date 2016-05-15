package core.scm;

/* Mutable vector */
public class SCMVector {

  private final Object[] vector;

  public SCMVector(Object[] array) {
    this.vector = array;
  }

  public Object get(int index) {
    return vector[index];
  }

  public void set(int index, Object value) {
    vector[index] = value;
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
      sb.append(e == this ? "(this Vector)" : e);
      if (i == (vector.length - 1)) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
    return sb.toString();
  }
}
