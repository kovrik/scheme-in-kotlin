package core.utils;

import core.exceptions.IllegalSyntaxException;
import core.procedures.math.Expt;
import core.procedures.math.Multiplication;
import core.procedures.math.ToExact;
import core.procedures.math.ToInexact;
import core.reader.Reader;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.scm.SCMSymbol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import static java.lang.Long.parseLong;

public final class NumberUtils {

  private NumberUtils() {}

  public static final int DEFAULT_SCALE = 16;
  public static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_EVEN;
  public static final MathContext DEFAULT_CONTEXT = new MathContext(DEFAULT_SCALE, ROUNDING_MODE);

  public static final BigDecimal E = new BigDecimal("2.71828182845904523536028747135266249775724709369995");

  private static final Pattern HASH_PATTERN = Pattern.compile(".+(#+\\.?+#?)/?(#+\\.?+#?)?$");

  private static final String EXPONENT_MARKS_PATTERN = "(s|l|d|e|f|S|L|D|E|F)";
  private static final String EXPONENT16_MARKS_PATTERN = "(s|l|S|L)";
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
      return SCMSymbol.of(number);
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
      return SCMSymbol.of(number);
    }

    /* Validate all digits */
    boolean hasAtLeastOneDigit = false;
    for (char c : n.toCharArray()) {
      /* Check if char is valid for this radix AND that we don't have # before digits */
      if (c != '/' && !isValidForRadix(c, radix) || (c == '#' && !hasAtLeastOneDigit)) {
        return SCMSymbol.of(number);
      }
      /* Check if it is a digit, not a hash/sign char */
      if ("#+-.".indexOf(c) == -1) {
        hasAtLeastOneDigit = true;
      }
    }
    if (!hasAtLeastOneDigit) {
      return SCMSymbol.of(number);
    }

    if (n.indexOf('#') > -1) {
      if (HASH_PATTERN.matcher(n).matches()) {
        n = n.replace('#', '0');
        exactness = exactness == null ? 'i' : exactness;
      } else {
        return SCMSymbol.of(number);
      }
    }

    /* Check if it is a rational number and if it is valid */
    int slashIndex = n.indexOf('/');
    if (slashIndex > -1 && (slashIndex != n.lastIndexOf('/') || n.indexOf('.') > -1)) {
      return SCMSymbol.of(number);
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
    if (!(re instanceof Number)) {
      return SCMSymbol.of(number);
    }

    String i = number.substring(p, number.length() - 1);
    if (i.length() == 1 && (i.charAt(0) == '+' || i.charAt(0) == '-')) {
      i += "1";
    }
    Object im = preProcessNumber(i, exactness, radix);
    if (!(im instanceof Number)) {
      return SCMSymbol.of(number);
    }
    return isZero(re) && isZero(im) ? 0L : new SCMBigComplex((Number)re, (Number)im);
  }

  /* Parse string into a number */
  private static Number processNumber(String number, Integer r, boolean exact, boolean useBigNum, Long exp) {
    Number result;
    int dot = number.indexOf('.');
    if (useBigNum) {
      /* Remove dot */
      number = number.replace(".", "");
      /* Process radix */
      BigDecimal bigDecimal = (r == 10) ? new BigDecimal(number) : new BigDecimal(new BigInteger(number, r));
      /* Process radix for a number with decimal point */
      if (dot > -1) {
        bigDecimal = bigDecimal.divide(BIG_DECIMAL_RADICES.get(r).pow(number.length() - dot), MathContext.UNLIMITED);
        if (bigDecimal.stripTrailingZeros().scale() == 0) {
          bigDecimal = bigDecimal.setScale(1, ROUNDING_MODE);
        }
      }
      result = bigDecimal;
    } else {
      if (dot > -1) {
        if (r == 10) {
          result = Double.parseDouble(number);
        } else {
          /* Remove dot */
          number = number.replace(".", "");
          result = parseLong(number, r) / Math.pow(r.doubleValue(), number.length() - dot);
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
    if (exact) {
      /* Racket's Reader does not convert into exact numbers 'properly':
       * #e2.3 returns 23/10
       * but (inexact->exact 2.3) returns 2589569785738035/1125899906842624
       *
       * Guile returns 2589569785738035/1125899906842624 in both cases.
       */
      if (isInexact(number)) {
        if (number instanceof Double) {
          BigDecimal bigDecimal = new BigDecimal(number.toString());
          int scale = bigDecimal.scale();
          return new SCMBigRational(bigDecimal.movePointRight(scale).toBigInteger(), BigInteger.TEN.pow(scale));
        }
        return ToExact.toExact(number);
      }
      return number;
    }
    return ToInexact.toInexact(number);
  }

  /* Parse string into a rational number */
  private static Number processRationalNumber(String numerator, String denominator, Integer r, boolean exact,
    boolean useBigNum, Long exp) {

    Number num = processNumber(numerator,   r, true, useBigNum, null);
    Number den = processNumber(denominator, r, true, useBigNum, null);
    SCMBigRational number = new SCMBigRational(num.toString(), den.toString());
    if (!exact) {
      Number result = ToInexact.toInexact(number);
      return exp == null ?  result : Multiplication.apply(result, Expt.expt(r, exp));
    }
    return number;
  }

  public static BigDecimal toBigDecimal(Number number) {
    if (number instanceof BigDecimal) {
      return (BigDecimal)number;
    }
    if (number instanceof SCMBigRational) {
      return ((SCMBigRational) number).toBigDecimal();
    }
    if (number instanceof SCMBigComplex) {
      throw new UnsupportedOperationException("undefined for complex!");
    }
    return new BigDecimal(number.toString());
  }

  public static boolean isRational(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof SCMBigComplex) {
      return false;
    }
    if (o instanceof Double) {
      return !Double.isInfinite((Double) o) && !Double.isNaN((Double) o);
    } else if (o instanceof Float) {
      return !Float.isInfinite((Float) o) && !Float.isNaN((Float) o);
    }
    return true;
  }

  public static boolean isExact(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long || o instanceof SCMBigRational || o instanceof Integer || o instanceof BigInteger) {
      return true;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).scale() == 0;
    }
    if (o instanceof SCMBigComplex) {
      return isExact(((SCMBigComplex) o).getRe()) && isExact(((SCMBigComplex) o).getIm());
    }
    return false;
  }

  public static boolean isInexact(Object o) {
    return !isExact(o);
  }

  public static boolean isInteger(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long || o instanceof Integer || o instanceof BigInteger) {
      return true;
    }
    if (o instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal)o;
      return bd.signum() == 0 || bd.scale() <= 0 || bd.stripTrailingZeros().scale() <= 0;
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).isDenominatorEqualToOne();
    }
    if (o instanceof Double) {
      return (Double)o == Math.floor((Double)o) && !Double.isInfinite((Double)o);
    }
    return false;
  }

  public static boolean isExactInteger(Object o) {
    return isExact(o) && isInteger(o);
  }

  public static boolean isZero(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long) {
      return Long.signum((Long)o) == 0;
    }
    if (o instanceof Double) {
      return Math.signum((Double)o) == 0.0;
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).signum() == 0;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).signum() == 0;
    }
    if (o instanceof Integer) {
      return Integer.signum((Integer)o) == 0;
    }
    if (o instanceof Float) {
      return Math.signum((Float)o) == 0;
    }
    if (o instanceof BigInteger) {
      return ((BigInteger)o).signum() == 0;
    }
    return false;
  }

  public static boolean isOne(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long) {
      return ((Long)o) == 1;
    }
    if (o instanceof Double) {
      return Double.compare((Double)o, 1d) == 0;
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).isOne();
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).compareTo(BigDecimal.ONE) == 0;
    }
    if (o instanceof Integer) {
      return ((Integer)o) == 1;
    }
    if (o instanceof Float) {
      return Float.floatToRawIntBits((Float)o) == 1;
    }
    if (o instanceof BigInteger) {
      return ((BigInteger)o).compareTo(BigInteger.ONE) == 0;
    }
    return false;
  }

  public static boolean isPositive(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long) {
      return Long.signum((Long)o) == 1;
    }
    if (o instanceof Double) {
      return Math.signum((Double)o) == 1.0;
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).signum() == 1;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).signum() == 1;
    }
    if (o instanceof Integer) {
      return Integer.signum((Integer)o) == 1;
    }
    if (o instanceof Float) {
      return Math.signum((Float)o) == 1;
    }
    if (o instanceof BigInteger) {
      return ((BigInteger)o).signum() == 1;
    }
    return false;
  }

  public static boolean isNegative(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long) {
      return Long.signum((Long)o) == -1;
    }
    if (o instanceof Double) {
      return Math.signum((Double)o) == -1.0;
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).signum() == -1;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).signum() == -1;
    }
    if (o instanceof Integer) {
      return Integer.signum((Integer)o) == -1;
    }
    if (o instanceof Float) {
      return Math.signum((Float)o) == -1;
    }
    if (o instanceof BigInteger) {
      return ((BigInteger)o).signum() == -1;
    }
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
    return !(o instanceof SCMBigComplex) && (o instanceof Number);
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

  /**
   * Tries to downcast big number to a smaller type (if possible)
   **/
  public static Number tryToDowncast(BigDecimal number) {
    /* Same checks are performed in longValueExact() method,
     * but we don't want exception to be thrown, just return the number */
    int d = number.precision() - number.scale();
    if (d <= 0 || d > 19) {
      return number;
    }
    if (isInteger(number)) {
      try {
        long smaller = number.longValueExact();
        if (isInexact(number)) {
          return (double)smaller;
        }
        return smaller;
      } catch (ArithmeticException e) {
        /* Down-casting has failed, ignore and return the original number */
      }
    }
    return number;
  }

  public static Number tryToDowncast(BigInteger number) {
    if (number.bitLength() <= 63) {
      try {
        return number.longValueExact();
      } catch (ArithmeticException e) {
        // ignore
      }
    }
    return new BigDecimal(number);
  }

  public static boolean isFinite(Number number) {
    if (number instanceof Double) {
      return Double.isFinite((Double) number);
    }
    if (number instanceof Float) {
      return Float.isFinite((Float) number);
    }
    return true;
  }

  public static boolean isNaN(Number number) {
    return number instanceof Double && Double.isNaN((Double) number);
  }
}
