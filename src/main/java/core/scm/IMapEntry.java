package core.scm;

import java.util.Map;

public interface IMapEntry extends Map.Entry {

  Object key();

  Object val();
}
