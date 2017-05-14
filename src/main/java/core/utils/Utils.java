package core.utils;

import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.math.Expt;
import core.procedures.math.Multiplication;
import core.procedures.math.ToExact;
import core.procedures.math.ToInexact;
import core.reader.Reader;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.scm.Symbol;
import core.writer.Writer;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import static java.lang.Long.parseLong;

public final class Utils {

  private Utils() {}

  public static final int DEFAULT_SCALE = 16;
  public static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_EVEN;
  public static final MathContext DEFAULT_CONTEXT = new MathContext(DEFAULT_SCALE, ROUNDING_MODE);

  public static final BigDecimal E = new BigDecimal("2.71828182845904523536028747135266249775724709369995");

  private static final Pattern HASH_PATTERN = Pattern.compile(".+(#+\\.?+#?)/?(#+\\.?+#?)?$");

  private static final String EXPONENT_MARKS_PATTERN = "[sldefSLDEF]";
  private static final String EXPONENT16_MARKS_PATTERN = "[slSL]";
  private static final Pattern EXPONENT_PATTERN = Pattern.compile(".+" + EXPONENT_MARKS_PATTERN + "[+-]?\\d+(\\.\\d*)?$");
  private static final Pattern EXPONENT16_PATTERN = Pattern.compile(".+" + EXPONENT16_MARKS_PATTERN + "[+-]?\\w+$");

  private static final Map<String, Number> SPECIAL_NUMBERS = new HashMap<>();
  static {
    SPECIAL_NUMBERS.put("+nan.0", Double.NaN);
    SPECIAL_NUMBERS.put("-nan.0", Double.NaN);
    SPECIAL_NUMBERS.put("+inf.0", Double.POSITIVE_INFINITY);
    SPECIAL_NUMBERS.put("-inf.0", Double.NEGATIVE_INFINITY);
  }

  private static final Map<Character, Integer> NAMED_RADICES = new HashMap<>();
  static {
    NAMED_RADICES.put('b',  2);
    NAMED_RADICES.put('B',  2);
    NAMED_RADICES.put('o',  8);
    NAMED_RADICES.put('O',  8);
    NAMED_RADICES.put('d',  10);
    NAMED_RADICES.put('D',  10);
    NAMED_RADICES.put('x',  16);
    NAMED_RADICES.put('X',  16);
    NAMED_RADICES.put(null, 10);
  }

  private static final Map<Integer, BigDecimal> BIG_DECIMAL_RADICES = new HashMap<>();
  static {
    IntStream.rangeClosed(2, 16).forEach(r -> BIG_DECIMAL_RADICES.put(r, new BigDecimal(r)));
  }

  public static int getRadixByChar(Character radixChar) {
    return NAMED_RADICES.get(radixChar);
  }

  /* Threshold after which we switch to BigDecimals */
  private static final Map<Integer, Integer> RADIX_THRESHOLDS = new HashMap<>();
  static {
    RADIX_THRESHOLDS.put(2,  63);
    RADIX_THRESHOLDS.put(3,  39);
    RADIX_THRESHOLDS.put(4,  31);
    RADIX_THRESHOLDS.put(5,  27);
    RADIX_THRESHOLDS.put(6,  24);
    RADIX_THRESHOLDS.put(7,  22);
    RADIX_THRESHOLDS.put(8,  21);
    RADIX_THRESHOLDS.put(9,  19);
    RADIX_THRESHOLDS.put(10, 18);
    RADIX_THRESHOLDS.put(11, 18);
    RADIX_THRESHOLDS.put(12, 17);
    RADIX_THRESHOLDS.put(13, 17);
    RADIX_THRESHOLDS.put(14, 16);
    RADIX_THRESHOLDS.put(15, 16);
    RADIX_THRESHOLDS.put(16, 15);
  }

  private static final Map<Integer, String> RADIX_CHARS = new HashMap<>();
  static {
    RADIX_CHARS.put(2,  "#+-.01");
    RADIX_CHARS.put(3,  RADIX_CHARS.get(2)  + "2");
    RADIX_CHARS.put(4,  RADIX_CHARS.get(3)  + "3");
    RADIX_CHARS.put(5,  RADIX_CHARS.get(4)  + "4");
    RADIX_CHARS.put(6,  RADIX_CHARS.get(5)  + "5");
    RADIX_CHARS.put(7,  RADIX_CHARS.get(6)  + "6");
    RADIX_CHARS.put(8,  RADIX_CHARS.get(7)  + "7");
    RADIX_CHARS.put(9,  RADIX_CHARS.get(8)  + "8");
    RADIX_CHARS.put(10, RADIX_CHARS.get(9)  + "9");
    RADIX_CHARS.put(11, RADIX_CHARS.get(10) + "aA");
    RADIX_CHARS.put(12, RADIX_CHARS.get(11) + "bB");
    RADIX_CHARS.put(13, RADIX_CHARS.get(12) + "cC");
    RADIX_CHARS.put(14, RADIX_CHARS.get(13) + "dD");
    RADIX_CHARS.put(15, RADIX_CHARS.get(14) + "eE");
    RADIX_CHARS.put(16, RADIX_CHARS.get(15) + "fF");
  }

  /* Check if digit is valid for a number in a specific radix */
  public static boolean isValidForRadix(char c, int radix) {
    return RADIX_CHARS.get(radix).indexOf(c) > -1;
  }

  /* Coerce to DECIMAL64 context if one of the numbers has non-zero scale */
  public static MathContext getMathContext(BigDecimal first, BigDecimal second) {
    return (first.scale() > 0 || second.scale() > 0) ? MathContext.DECIMAL64 : MathContext.UNLIMITED;
  }

  // FIXME Simplify and cleanup!
  /* Check if string represents a valid number and process it */
  public static Object preProcessNumber(final String number, Character exactness, int radix) {
    /* First check if it is a special number */
    Number special = SPECIAL_NUMBERS.get(number);
    if (special != null) {
      return special;
    }
    /* Check if that is a complex number (ends with `i` or `I`) */
    char last = number.charAt(number.length() - 1);
    if (last == 'i' || last == 'I') {
      return processComplexNumber(number, exactness, radix);
    }
    /* Multiple decimal points are not allowed*/
    if (number.indexOf('.') != number.lastIndexOf('.')) {
      return Symbol.intern(number);
    }

    /* Read exponent mark if present */
    Pattern exponentPattern = EXPONENT_PATTERN;
    String exponentMarksPattern = EXPONENT_MARKS_PATTERN;
    if (radix == 16) {
      exponentPattern = EXPONENT16_PATTERN;
      exponentMarksPattern = EXPONENT16_MARKS_PATTERN;
    }
    Long exp = null;
    String n = number;
    if (exponentPattern.matcher(number).matches()) {
      String[] split = number.split(exponentMarksPattern);
      n = split[0];
      String exponent = split[1];
      try {
        Number e = processNumber(exponent, radix, true, false, null);
        if (!(e instanceof Long)) {
          throw new IllegalSyntaxException("read: bad exponent: " + number);
        }
        exp = (Long) e;
      } catch (NumberFormatException ex) {
        throw new IllegalSyntaxException("read: bad exponent: " + number);
      }
      exactness = exactness == null ? 'i' : exactness;
    }
    /* Validate sign */
    if ((n.lastIndexOf('+') > 0) || (n.lastIndexOf('-') > 0)) {
      return Symbol.intern(number);
    }

    /* Validate all digits */
    boolean hasAtLeastOneDigit = false;
    for (char c : n.toCharArray()) {
      /* Check if char is valid for this radix AND that we don't have # before digits */
      if (c != '/' && !isValidForRadix(c, radix) || (c == '#' && !hasAtLeastOneDigit)) {
        return Symbol.intern(number);
      }
      /* Check if it is a digit, not a hash/sign char */
      if ("#+-.".indexOf(c) == -1) {
        hasAtLeastOneDigit = true;
      }
    }
    if (!hasAtLeastOneDigit) {
      return Symbol.intern(number);
    }

    if (n.indexOf('#') > -1) {
      if (HASH_PATTERN.matcher(n).matches()) {
        n = n.replace('#', '0');
        exactness = exactness == null ? 'i' : exactness;
      } else {
        return Symbol.intern(number);
      }
    }

    /* Check if it is a rational number and if it is valid */
    int slashIndex = n.indexOf('/');
    if (slashIndex > -1 && (slashIndex != n.lastIndexOf('/') || n.indexOf('.') > -1)) {
      return Symbol.intern(number);
    }

    /* Rational and Integral numbers are exact by default */
    boolean isIntegral = n.indexOf('.') < 0;
    boolean exact = exactness != null ? Reader.isExact(exactness) : slashIndex > -1 || isIntegral;

    Integer threshold = RADIX_THRESHOLDS.get(radix);
    int hasSign = (n.charAt(0) == '-' || n.charAt(0) == '+') ? 1 : 0;
    if (slashIndex > -1) {
      String numerator = n.substring(0, slashIndex);
      String denominator = n.substring(slashIndex + 1);
      boolean useBigNum = (numerator.length() > (threshold + hasSign)) || (denominator.length() > (threshold + hasSign));
      return processRationalNumber(numerator, denominator, radix, exact, useBigNum, exp);
    }
    boolean useBigNum = n.length() > threshold + hasSign;
    return processNumber(n, radix, exact, useBigNum, exp);
  }

  private static Object processComplexNumber(String number, Character exactness, int radix) {
    /* Assume that we have a complex number and try to parse it */
    int p = Math.max(number.lastIndexOf('+'), number.lastIndexOf('-'));
    String r = number.substring(0, p);
    Object re = 0L;
    if (!r.isEmpty()) {
      re = preProcessNumber(r, exactness, radix);
    }
    if (!isReal(re)) {
      return Symbol.intern(number);
    }

    String i = number.substring(p, number.length() - 1);
    if (i.length() == 1 && (i.charAt(0) == '+' || i.charAt(0) == '-')) {
      i += "1";
    }
    Object im = preProcessNumber(i, exactness, radix);
    if (!isReal(im)) {
      return Symbol.intern(number);
    }
    return isZero(re) && isZero(im) ? 0L : new BigComplex(toBigDecimal((Number) re), toBigDecimal((Number)im));
  }

  /* Parse string into a number */
  private static Number processNumber(String number, Integer r, boolean exact, boolean useBigNum, Long exp) {
    Number result;
    int dotPos = number.indexOf('.');
    if (useBigNum) {
      if (dotPos < 0) {
        result = new BigInteger(number, r);
      } else {
        /* Remove dot */
        number = number.replace(".", "");
        /* Process radix */
        BigDecimal bigDecimal = r == 10 ? new BigDecimal(number) : new BigDecimal(new BigInteger(number, r));
        /* Process radix for a number with decimal point */
        bigDecimal = bigDecimal.divide(BIG_DECIMAL_RADICES.get(r).pow(number.length() - dotPos), MathContext.UNLIMITED);
        if (bigDecimal.stripTrailingZeros().scale() == 0) {
          bigDecimal = bigDecimal.setScale(1, ROUNDING_MODE);
        }
        result = bigDecimal;
      }
    } else {
      if (dotPos > -1) {
        if (r == 10) {
          result = Double.parseDouble(number);
        } else {
          /* Remove dot */
          number = number.replace(".", "");
          result = parseLong(number, r) / Math.pow(r.doubleValue(), number.length() - dotPos);
        }
      } else {
        result = Long.parseLong(number, r);
      }
    }
    if (exp != null && !isZero(exp)) {
      if (exp > 999999) {
        return isPositive(result) ? Double.POSITIVE_INFINITY : Double.NEGATIVE_INFINITY;
      } else if (exp < -999) {
        return isPositive(result) ? 0d : -0d;
      }
      if (r == 10 && !exact) {
        return Double.parseDouble(result + "E" + exp);
      } else {
        result = Multiplication.apply(result, Expt.expt(r.longValue(), exp));
      }
    }
    return processExactness(result, exact);
  }

  private static Number processExactness(Number number, boolean exact) {
    if (!exact) {
      return ToInexact.toInexact(number);
    }
    /* Racket's Reader does not convert into exact numbers 'properly':
     * #e2.3 returns 23/10
     * but (inexact->exact 2.3) returns 2589569785738035/1125899906842624
     * Guile returns 2589569785738035/1125899906842624 in both cases.
     */
    if (isInexact(number)) {
      if (number instanceof Double) {
        BigDecimal bigDecimal = toBigDecimal(number);
        int scale = bigDecimal.scale();
        return BigRatio.valueOf(bigDecimal.movePointRight(scale).toBigInteger(), BigInteger.TEN.pow(scale));
      }
      return ToExact.toExact(number);
    }
    return number;
  }

  /* Parse string into a rational number */
  private static Number processRationalNumber(String numerator, String denominator, Integer r, boolean exact,
    boolean useBigNum, Long exp) {

    Number num = processNumber(numerator,   r, true, useBigNum, null);
    Number den = processNumber(denominator, r, true, useBigNum, null);
    BigRatio number = BigRatio.valueOf(num.toString(), den.toString());
    if (!exact) {
      Number result = ToInexact.toInexact(number);
      return exp == null ?  result : Multiplication.apply(result, Expt.expt(r, exp));
    }
    return number;
  }

  public static BigDecimal toBigDecimal(Number number) {
    Class clazz = number.getClass();
    if (clazz == BigDecimal.class) return (BigDecimal) number;
    if (clazz == Long.class)       return BigDecimal.valueOf((Long) number);
    if (clazz == BigInteger.class) return new BigDecimal((BigInteger) number);
    if (clazz == Double.class)     return BigDecimal.valueOf((Double) number);
    if (clazz == BigRatio.class)   return ((BigRatio) number).toBigDecimal();
    if (clazz == BigComplex.class) throw new UnsupportedOperationException("undefined for complex!");
    return new BigDecimal(number.toString());
  }

  public static BigInteger toBigInteger(Number number) {
    Class clazz = number.getClass();
    if (clazz == BigInteger.class) return (BigInteger) number;
    if (clazz == Long.class)       return BigInteger.valueOf((Long) number);
    if (clazz == Double.class)     return BigInteger.valueOf(number.longValue());
    if (clazz == BigComplex.class) throw new UnsupportedOperationException("undefined for complex!");
    return new BigInteger(number.toString());
  }

  public static boolean isRational(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    Class clazz = o.getClass();
    if (clazz == BigComplex.class) {
      return false;
    }
    if (clazz == Double.class) {
      return !Double.isInfinite((Double) o) && !Double.isNaN((Double) o);
    } else if (clazz == Float.class) {
      return !Float.isInfinite((Float) o) && !Float.isNaN((Float) o);
    }
    return true;
  }

  public static boolean isExact(Object o) {
    if (o == null) {
      return false;
    }
    Class clazz = o.getClass();
    if (clazz == Long.class || clazz == BigRatio.class || clazz == Integer.class ||
        clazz == BigInteger.class || clazz == Short.class || clazz == Byte.class) {

      return true;
    }
    if (clazz == BigDecimal.class) {
      return ((BigDecimal)o).scale() == 0;
    }
    if (clazz == BigComplex.class) {
      return isExact(((BigComplex) o).getRe()) && isExact(((BigComplex) o).getIm());
    }
    return false;
  }

  public static boolean isInexact(Object o) {
    return !isExact(o);
  }

  public static boolean isInteger(Object o) {
    if (o == null) {
      return false;
    }
    Class clazz = o.getClass();
    if (clazz == Long.class || clazz == Integer.class || clazz == BigInteger.class || clazz == Short.class || clazz == Byte.class) {
      return true;
    }
    if (clazz == BigDecimal.class) {
      BigDecimal bd = (BigDecimal)o;
      return bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;
    }
    if (clazz == BigRatio.class) {
      return ((BigRatio)o).isDenominatorEqualToOne();
    }
    if (clazz == Double.class) {
      return (Double)o == Math.floor((Double)o) && !Double.isInfinite((Double)o);
    }
    return false;
  }

  public static boolean isExactInteger(Object o) {
    return isExact(o) && isInteger(o);
  }

  public static boolean isZero(Object o) {
    if (o == null) return false;
    Class clazz = o.getClass();
    if (clazz == Long.class)       return Long.signum((Long) o) == 0;
    if (clazz == Double.class)     return Math.signum((Double) o) == 0.0;
    if (clazz == BigRatio.class)   return ((BigRatio) o).signum() == 0;
    if (clazz == BigDecimal.class) return ((BigDecimal) o).signum() == 0;
    if (clazz == Integer.class)    return Integer.signum((Integer) o) == 0;
    if (clazz == Short.class)      return Integer.signum((Short) o) == 0;
    if (clazz == Byte.class)       return Integer.signum((Byte) o) == 0;
    if (clazz == Float.class)      return Math.signum((Float) o) == 0;
    if (clazz == BigInteger.class) return ((BigInteger) o).signum() == 0;
    return false;
  }

  public static boolean isOne(Object o) {
    if (o == null) return false;
    Class clazz = o.getClass();
    if (clazz == Long.class)       return ((Long) o) == 1;
    if (clazz == Double.class)     return Double.compare((Double) o, 1d) == 0;
    if (clazz == BigRatio.class)   return ((BigRatio) o).isOne();
    if (clazz == BigDecimal.class) return ((BigDecimal) o).compareTo(BigDecimal.ONE) == 0;
    if (clazz == Integer.class)    return ((Integer) o) == 1;
    if (clazz == Short.class)      return (Short) o == 1;
    if (clazz == Byte.class)       return (Byte) o == 1;
    if (clazz == Float.class)      return Float.floatToRawIntBits((Float) o) == 1;
    if (clazz == BigInteger.class) return ((BigInteger) o).compareTo(BigInteger.ONE) == 0;
    return false;
  }

  public static boolean isPositive(Object o) {
    if (o == null) return false;
    Class clazz = o.getClass();
    if (clazz == Long.class)       return Long.signum((Long) o) == 1;
    if (clazz == Double.class)     return Math.signum((Double) o) == 1.0;
    if (clazz == BigRatio.class)   return ((BigRatio) o).signum() == 1;
    if (clazz == BigDecimal.class) return ((BigDecimal) o).signum() == 1;
    if (clazz == Integer.class)    return Integer.signum((Integer) o) == 1;
    if (clazz == Short.class)      return Integer.signum((Short) o) == 1;
    if (clazz == Byte.class)       return Integer.signum((Byte) o) == 1;
    if (clazz == Float.class)      return Math.signum((Float) o) == 1;
    if (clazz == BigInteger.class) return ((BigInteger) o).signum() == 1;
    return false;
  }

  public static boolean isNegative(Object o) {
    if (o == null) return false;
    Class clazz = o.getClass();
    if (clazz == Long.class)       return Long.signum((Long) o) == -1;
    if (clazz == Double.class)     return Math.signum((Double) o) == -1.0;
    if (clazz == BigRatio.class)   return ((BigRatio) o).signum() == -1;
    if (clazz == BigDecimal.class) return ((BigDecimal) o).signum() == -1;
    if (clazz == Integer.class)    return Integer.signum((Integer) o) == -1;
    if (clazz == Short.class)      return Integer.signum((Short) o) == -1;
    if (clazz == Byte.class)       return Integer.signum((Byte) o) == -1;
    if (clazz == Float.class)      return Math.signum((Float) o) == -1;
    if (clazz == BigInteger.class) return ((BigInteger) o).signum() == -1;
    return false;
  }

  public static boolean isNonNegative(Object o) {
    return !isNegative(o);
  }

  public static boolean isExactPositiveInteger(Object o) {
    return isExact(o) && isInteger(o) && isPositive(o);
  }

  public static boolean isExactNonNegativeInteger(Object o) {
    return isExact(o) && isInteger(o) && isNonNegative(o);
  }

  public static boolean isReal(Object o) {
    return !(o instanceof BigComplex) && (o instanceof Number);
  }

  /**
   * Inexactness 'taint'
   * Computations that involve an inexact number produce inexact results,
   * so that inexactness acts as a kind of taint on numbers.
   * See https://docs.racket-lang.org/guide/numbers.html
   */
  public static Number inexactnessTaint(Number result, Number other) {
    return isInexact(other) && isExact(result) ? ToInexact.toInexact(result) : result;
  }

  public static Number downcastNumber(Number number) {
    /* Try to downcast Rationals with denominator = 1 */
    if ((number instanceof BigRatio) && (((BigRatio) number).isDenominatorEqualToOne())) {
      return tryDowncast((BigRatio) number);
    }
    /* Try to downcast Big Numbers */
    if (number instanceof BigDecimal) return tryDowncast((BigDecimal) number);
    if (number instanceof BigInteger) return tryDowncast((BigInteger) number);
    return number;
  }

  /**
   * Tries to downcast big number to a smaller type (if possible)
   **/
  private static Number tryDowncast(BigDecimal number) {
    /* Same checks are performed in longValueExact() method,
     * but we don't want exception to be thrown, just return the number */
    if (!isInteger(number)) {
      return number;
    }
    try {
      return number.longValueExact();
    } catch (ArithmeticException e) {
      /* Down-casting has failed, ignore and cast to BigInteger then */
      return number.toBigInteger();
    }
  }

  /**
   * Tries to downcast big number to a smaller type (if possible)
   **/
  private static Number tryDowncast(BigInteger number) {
    /* Same checks are performed in longValueExact() method,
     * but we don't want exception to be thrown, just return the number */
    if (number.bitLength() <= 63) {
      return number.longValue();
    }
    if (isInteger(number)) {
      try {
        return number.longValueExact();
      } catch (ArithmeticException e) {
        /* Down-casting has failed, ignore and return the original number */
      }
    }
    return number;
  }

  private static Number tryDowncast(BigRatio bigRatio) {
    return tryDowncast(bigRatio.getNumerator());
  }

  public static boolean isFinite(Number number) {
    if (number == null) {
      return true;
    } else if (number.getClass() == Double.class) {
      return Double.isFinite((Double) number);
    } else if (number.getClass() == Float.class) {
      return Float.isFinite((Float) number);
    }
    return true;
  }

  public static boolean isNaN(Number number) {
    return number != null && number.getClass() == Double.class && Double.isNaN((Double) number);
  }

  /* Upcast number if required */
  public static Number upcast(Number number) {
    if (number == null) {
      return null;
    }
    Class clazz = number.getClass();
    if ((clazz == Byte.class) || (clazz == Short.class) || (clazz == Integer.class)) {
      return number.longValue();
    } else if (clazz == Float.class) {
      return number.doubleValue();
    }
    return number;
  }

  public static boolean isBitOpSupported(Object obj) {
    if (!(obj instanceof Byte || obj instanceof Short || obj instanceof Integer || obj instanceof Long)) {
      throw new WrongTypeException("bit operation not supported for: " + Writer.write(obj));
    }
    return true;
  }

  /**
   * Converts any Object to boolean.
   * Returns FALSE only if value is FALSE itself or null.
   * Returns TRUE otherwise.
   */
  public static boolean toBoolean(Object value) {
    return value instanceof Boolean ? (boolean)value : value != null;
  }

  public static boolean isSeqable(Object obj) {
    return obj == null || obj instanceof Iterable || obj instanceof CharSequence;
  }

  // TODO Return custom Sequence object instead of Iterator
  // TODO iterator for Maps (MapEntries)?
  public static Iterator toIterator(Object obj) {
    if (!isSeqable(obj)) {
      throw new RuntimeException("don't know how to create Sequence from " + obj.getClass());
    }
    if (obj instanceof Iterable) {
      return ((Iterable) obj).iterator();
    } else if (obj instanceof CharSequence) {
      return stringIterator((CharSequence) obj);
    }
    return Collections.EMPTY_LIST.iterator();
  }

  /* Returns String Iterator */
  private static Iterator<Character> stringIterator(final CharSequence string) {
    if (string == null) throw new NullPointerException();
    return new Iterator<Character>() {

      private int index = 0;

      @Override
      public boolean hasNext() {
        return index < string.length();
      }

      @Override
      public Character next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        return string.charAt(index++);
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }
}
