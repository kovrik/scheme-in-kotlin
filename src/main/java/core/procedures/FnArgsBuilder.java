package core.procedures;

public class FnArgsBuilder {

  private int min = 0;
  /* JVM restricts max number of arguments to 255
   * If max is more than 255, then we assume that function accepts ANY number of arguments.
   * If max is less or equal to 255, then we assume that function accepts exactly up to max arguments.
   **/
  private int max = Integer.MAX_VALUE;
  private Class<?>[] mandatory = new Class[]{};
  private Class<?> rest = null;
  private Class<?> last = null;

  public FnArgs build() {
    return new FnArgs(min, max, mandatory, rest, last);
  }

  public FnArgsBuilder min(int min) {
    this.min = min;
    return this;
  }

  public FnArgsBuilder max(int max) {
    this.max = max;
    return this;
  }

  public FnArgsBuilder mandatory(Class<?>[] mandatoryTypes) {
    this.mandatory = mandatoryTypes;
    return this;
  }

  public FnArgsBuilder rest(Class<?> restType) {
    this.rest = restType;
    return this;
  }

  public FnArgsBuilder last(Class<?> lastType) {
    this.last = lastType;
    return this;
  }
}
