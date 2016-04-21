package main.core.math.numeric;

import main.core.math.IOperation;

public interface INumericalOperation<T extends Number> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
