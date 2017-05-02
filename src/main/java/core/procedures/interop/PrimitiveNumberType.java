package core.procedures.interop;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class PrimitiveNumberType extends AFn {

  public static final PrimitiveNumberType BYTE   = new PrimitiveNumberType(byte.class);
  public static final PrimitiveNumberType SHORT  = new PrimitiveNumberType(short.class);
  public static final PrimitiveNumberType INT    = new PrimitiveNumberType(int.class);
  public static final PrimitiveNumberType LONG   = new PrimitiveNumberType(long.class);
  public static final PrimitiveNumberType DOUBLE = new PrimitiveNumberType(double.class);
  public static final PrimitiveNumberType FLOAT  = new PrimitiveNumberType(float.class);

  private final String name;
  private final Class clazz;

  private PrimitiveNumberType(Class clazz) {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
    this.name = clazz.getSimpleName();
    this.clazz = clazz;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return name;
  }

  /* FIXME Have to box it */
  @Override
  public Number apply1(Object arg) {
    Number number = (Number)arg;
    if (clazz == byte.class) {
      return number.byteValue();
    } else if (clazz == short.class) {
      return number.shortValue();
    } else if (clazz == int.class) {
      return number.intValue();
    } else if (clazz == long.class) {
      return number.longValue();
    } else if (clazz == double.class) {
      return number.doubleValue();
    } else if (clazz == float.class) {
      return number.floatValue();
    }
    throw new IllegalArgumentException("Unknown primitive type: " + clazz);
  }
}
