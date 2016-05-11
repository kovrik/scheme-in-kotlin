package main.core.procedures.math.bool;

import main.core.procedures.math.IOperation;

public interface IBooleanOperation<T extends Boolean> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
