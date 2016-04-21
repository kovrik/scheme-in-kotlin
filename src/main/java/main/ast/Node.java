package main.ast;

//public class Node {
//
//  public enum Type {
//    LIST,
//    NUMBER,
//    STRING,
//    META,
//    SYMBOL;
//  }
//
//  private final Type type;
//  private final Object value;
//
//  public Node(Type type, Object value) {
//    if (type == null) {
//      throw new IllegalArgumentException("Type cannot be null!");
//    }
//    this.type = type;
//    this.value = value;
//  }
//
//  public Type getType() {
//    return type;
//  }
//
//  public Object getValue() {
//    return value;
//  }
//
//  @Override
//  public boolean equals(Object o) {
//
//    if (this == o) return true;
//    if (o == null || getClass() != o.getClass()) return false;
//
//    Node node = (Node) o;
//
//    if (type != node.type) return false;
//    return value != null ? value.equals(node.value) : node.value == null;
//  }
//
//  @Override
//  public int hashCode() {
//    int result = type != null ? type.hashCode() : 0;
//    result = 31 * result + (value != null ? value.hashCode() : 0);
//    return result;
//  }
//
//  // FIXME Use toString instead of Print
//  @Override
//  public String toString() {
//    return value.toString();
//  }
//}
