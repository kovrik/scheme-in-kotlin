package core.scm;

/* Immutable Vector */
public class ImmutableVector extends Vector {

  public ImmutableVector(Object... elements) {
    super(elements);
  }

  @Override
  public Type getType() {
    return Type.IMMUTABLE_VECTOR;
  }
}
