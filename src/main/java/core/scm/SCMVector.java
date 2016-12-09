package core.scm;

public abstract class SCMVector implements ISCMClass {

  public abstract Object get(int index);

  public abstract int length();

  public abstract Object[] getArray();

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.VECTOR;
  }
}
