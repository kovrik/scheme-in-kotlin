package core.procedures;

import core.scm.ISCMClass;

public interface IFn extends ISCMClass {

  Object invoke(Object... args);
}
