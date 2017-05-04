package core.scm;

/* Mutable Vector */
public class MutableVector extends Vector {

  public MutableVector() {
    super();
  }

  public MutableVector(int size, Object init) {
    super(size, init);
  }

  public MutableVector(Object... elements) {
    super((Object[])elements);
  }

  public MutableVector(Object e) {
    super(e);
  }

  public void set(int index, Object value) {
    getArray()[index] = value;
  }

  @Override
  public Type getType() {
    return Type.MUTABLE_VECTOR;
  }
}

