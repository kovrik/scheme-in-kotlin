package core.scm;

/* Immutable Vector */
public class SCMImmutableVector extends SCMVector {

  public SCMImmutableVector(Object... elements) {
    super(elements);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.IMMUTABLE_VECTOR;
  }
}
