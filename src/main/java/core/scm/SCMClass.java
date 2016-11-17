package core.scm;

public class SCMClass implements ISCMClass {

  /* Java Classes */
  public static final SCMClass INTEGER     = new SCMClass("Integer");
  public static final SCMClass DOUBLE      = new SCMClass("Double");
  public static final SCMClass STRING      = new SCMClass("String");
  public static final SCMClass CHARACTER   = new SCMClass("Character");
  public static final SCMClass BOOLEAN     = new SCMClass("Boolean");

  /* Custom SCM Classes */
  public static final SCMClass ENVIRONMENT = new SCMClass("Environment");
  public static final SCMClass SPECIALFORM = new SCMClass("SpecialForm");
  public static final SCMClass NIL         = new SCMClass("Nil");
  public static final SCMClass PAIR        = new SCMClass("Pair");
  public static final SCMClass SYMBOL      = new SCMClass("Symbol");
  public static final SCMClass VECTOR      = new SCMClass("Vector");
  public static final SCMClass PROMISE     = new SCMClass("Promise");
  public static final SCMClass PROCEDURE   = new SCMClass("Procedure");
  public static final SCMClass ERROR       = new SCMClass("Error");
  public static final SCMClass CLASS       = new SCMClass("Class");
  public static final SCMClass INPUT_PORt  = new SCMClass("InputPort");
  public static final SCMClass OUTPUT_PORt = new SCMClass("OutputPort");
  public static final SCMClass EOF         = new SCMClass("EOF");

  public static final SCMClass UNSPECIFIED = new SCMClass("Unspecified");

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
