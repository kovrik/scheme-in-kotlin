package core.scm;

import core.exceptions.WrongTypeException;

public class SCMClass implements ISCMClass {

  /* Java Classes */
  public static final SCMClass INTEGER     = new SCMClass("Integer");
  public static final SCMClass DOUBLE      = new SCMClass("Double");
  public static final SCMClass RATIONAL    = new SCMClass("Rational");
  public static final SCMClass STRING      = new SCMClass("String");
  public static final SCMClass CHARACTER   = new SCMClass("Character");
  public static final SCMClass BOOLEAN     = new SCMClass("Boolean");

  /* Custom SCM Classes */
  public static final SCMClass ENVIRONMENT = new SCMClass("Environment");
  public static final SCMClass SPECIALFORM = new SCMClass("SpecialForm");
  public static final SCMClass NIL         = new SCMClass("Nil");
  public static final SCMClass LIST        = new SCMClass("List");
  public static final SCMClass PAIR        = new SCMClass("Pair");
  public static final SCMClass SYMBOL      = new SCMClass("Symbol");
  public static final SCMClass VECTOR      = new SCMClass("Vector");
  public static final SCMClass PROMISE     = new SCMClass("Promise");
  public static final SCMClass PROCEDURE   = new SCMClass("Procedure");
  public static final SCMClass ERROR       = new SCMClass("Error");
  public static final SCMClass CLASS       = new SCMClass("Class");
  public static final SCMClass INPUT_PORT  = new SCMClass("InputPort");
  public static final SCMClass OUTPUT_PORT = new SCMClass("OutputPort");
  public static final SCMClass EOF         = new SCMClass("EOF");

  public static final SCMClass UNSPECIFIED = new SCMClass("Unspecified");

  public static boolean assertClass(Object o, Class c) {
    if (c.isAssignableFrom(o.getClass())) {
      return true;
    }
    throw new WrongTypeException(c.getSimpleName(), o);
  }

  public static boolean checkClass(Class<?> expected, Class<?> actual) {
    if (expected == actual) {
      return true;
    }
    if (expected.isAssignableFrom(actual)) {
      return true;
    }
    if (ISCMClass.class.isAssignableFrom(actual)) {
      if (String.class.equals(expected) && SCMString.class.equals(actual)) {
        return true;
      } else if (Boolean.class.equals(expected) && SCMBoolean.class.equals(actual)) {
        return true;
      }
    }
    return false;
  }

  private final String name;

  public SCMClass(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  @Override
  public SCMClass getSCMClass() {
    return CLASS;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SCMClass scmClass = (SCMClass) o;
    return name != null ? name.equals(scmClass.name) : scmClass.name == null;
  }

  @Override
  public int hashCode() {
    return name != null ? name.hashCode() : 0;
  }

  @Override
  public String toString() {
    return "<class " + getName() + ">";
  }
}
