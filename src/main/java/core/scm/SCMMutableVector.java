package core.scm;

/* Mutable Vector */
public class SCMMutableVector extends SCMVector {

  public SCMMutableVector() {
    super();
  }

  public SCMMutableVector(int size, Object init) {
    super((int)size, (Object)init);
  }

  public SCMMutableVector(Object... elements) {
    super((Object[])elements);
  }

  public SCMMutableVector(Object e) {
    super(e);
  }

  public void set(int index, Object value) {
    getArray()[index] = value;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.MUTABLE_VECTOR;
  }
}

