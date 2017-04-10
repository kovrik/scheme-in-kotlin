package core.scm;

public class SCMMapEntry implements IMapEntry, ISCMClass {

  private final Object key;
  private final Object val;

  public SCMMapEntry(Object key, Object val) {
    this.key = key;
    this.val = val;
  }

  @Override
  public Object key() {
    return key;
  }

  @Override
  public Object val() {
    return val;
  }

  @Override
  public Object getKey() {
    return key;
  }

  @Override
  public Object getValue() {
    return val;
  }

  @Override
  public Object setValue(Object value) {
    throw new UnsupportedOperationException();
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.MAP_ENTRY;
  }

  @Override
  public String toString() {
    return "[" + key + " " + val + "]";
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SCMMapEntry that = (SCMMapEntry) o;
    if (key != null ? !key.equals(that.key) : that.key != null) return false;
    return val != null ? val.equals(that.val) : that.val == null;
  }

  @Override
  public int hashCode() {
    int result = key != null ? key.hashCode() : 0;
    result = 31 * result + (val != null ? val.hashCode() : 0);
    return result;
  }
}
