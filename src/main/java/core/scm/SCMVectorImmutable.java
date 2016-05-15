package core.scm;

/* Immutable vector */
public class SCMVectorImmutable extends SCMVector {

  public SCMVectorImmutable(Object[] array) {
    super(array);
  }

  @Override
  public void set(int index, Object value) {
    throw new UnsupportedOperationException("Can't set value in immutable vector!");
  }
}
