package core.procedures.math.numeric;

import core.procedures.math.IOperation;

public interface INumericalOperation<T extends Number> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
