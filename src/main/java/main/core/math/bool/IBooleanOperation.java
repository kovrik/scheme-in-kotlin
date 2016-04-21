package main.core.math.bool;

import main.core.math.IOperation;

public interface IBooleanOperation<T extends Boolean> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
