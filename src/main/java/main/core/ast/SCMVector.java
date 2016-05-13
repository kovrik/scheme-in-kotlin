package main.core.ast;

public class SCMVector {

  private final Object[] arr;

  public SCMVector(Object[] array) {
    this.arr = array;
  }

  @Override
  public String toString() {
    if (arr.length == 0) {
      return "#()";
    }
    StringBuilder sb = new StringBuilder();
    sb.append("#(");
    for (int i = 0; i < arr.length; i++) {
      Object e = arr[i];
      sb.append(e == this ? "(this List)" : e);
      if (i == (arr.length - 1)) {
        return sb.append(')').toString();
      }
      sb.append(' ');
    }
    return sb.toString();
  }
}
