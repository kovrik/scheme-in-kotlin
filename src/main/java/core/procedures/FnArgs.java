package core.procedures;

final class FnArgs {

  /* TODO Replace with TypeChecker OR Contracts */

  private final int min;
  /* JVM restricts max number of arguments to 255
   * If max is more than 255, then we assume that function accepts ANY number of arguments.
   * If max is less or equal to 255, then we assume that function accepts exactly up to max arguments.
   **/
  private final int max;
  private final Class<?>[] mandatory;
  private final Class<?> rest;
  private final Class<?> last;

  FnArgs(int min, int max, Class<?>[] mandatory, Class<?> rest, Class<?> last) {
    this.min = min;
    this.max = max;
    this.mandatory = mandatory;
    this.rest = rest;
    this.last = last;
  }

  public int min() {
    return min;
  }

  public int max() {
    return max;
  }

  public Class<?>[] mandatory() {
    return mandatory;
  }

  public Class<?> rest() {
    return rest;
  }

  public Class<?> last() {
    return last;
  }
}
