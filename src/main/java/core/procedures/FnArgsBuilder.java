package core.procedures;

public class FnArgsBuilder {

  private int minArgs = 0;
  private int maxArgs = 255;
  private Class<?>[] mandatoryArgsTypes = new Class[]{};
  private Class<?> restArgsType = null;
  private Class<?> lastArgType = null;

  public int getMinArgs() {
    return minArgs;
  }

  public FnArgsBuilder minArgs(int minArgs) {
    this.minArgs = minArgs;
    return this;
  }

  public int getMaxArgs() {
    return maxArgs;
  }

  public FnArgsBuilder maxArgs(int maxArgs) {
    this.maxArgs = maxArgs;
    return this;
  }

  public Class<?>[] getMandatoryArgsTypes() {
    return mandatoryArgsTypes;
  }

  public FnArgsBuilder mandatoryArgsTypes(Class<?>[] mandatoryArgsTypes) {
    this.mandatoryArgsTypes = mandatoryArgsTypes;
    return this;
  }

  public Class<?> getRestArgsType() {
    return restArgsType;
  }

  public FnArgsBuilder restArgsType(Class<?> restArgsType) {
    this.restArgsType = restArgsType;
    return this;
  }

  public Class<?> getLastArgType() {
    return lastArgType;
  }

  public FnArgsBuilder lastArgType(Class<?> lastArgType) {
    this.lastArgType = lastArgType;
    return this;
  }
}
