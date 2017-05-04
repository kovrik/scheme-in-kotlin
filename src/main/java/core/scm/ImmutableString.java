package core.scm;

/**
 * Marker class for Immutable String.
 * Internally, Java String is used instead.
 */
public final class ImmutableString implements ITyped {

  private ImmutableString() {}

  @Override
  public Type getType() {
    return Type.IMMUTABLE_STRING;
  }
}
