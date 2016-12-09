package core.scm;

/**
 * Marker class for Immutable String.
 * Internally, Java String is used instead.
 */
public final class SCMImmutableString implements ISCMClass {

  private SCMImmutableString() {}

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.IMMUTABLE_STRING;
  }
}
