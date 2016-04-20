package main.functions.math;

public interface IBooleanOperation<T extends Boolean> extends IOperation<T> {

  T zero();

  T apply(T first, T second);
}
