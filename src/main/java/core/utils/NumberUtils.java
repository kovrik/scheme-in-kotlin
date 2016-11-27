package core.utils;

import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.reader.parsers.StringParser;
import core.scm.SCMBigRational;
import core.scm.SCMSymbol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import static java.lang.Long.parseLong;

public class NumberUtils {

  private NumberUtils() {}

  public static final MathContext DEFAULT_CONTEXT = MathContext.DECIMAL64;

  public static final StringParser EXACTNESS = StringParser.choice("#e", "#i", "#E", "#I");
  public static final StringParser RADIX = StringParser.choice("#b", "#o", "#d", "#x", "#B", "#O", "#D", "#X");

  public static final StringParser EXACTNESS_RADIX = EXACTNESS.andThenMaybe(RADIX);
  public static final StringParser RADIX_EXACTNESS = RADIX.andThenMaybe(EXACTNESS);

  private static final Pattern HASH_PATTERN = Pattern.compile(".+(#+\\.?+#?)/?(#+\\.?+#?)?$");

  public static final Map<String, Number> SPECIAL_NUMBERS = new HashMap<>();
  static {
    SPECIAL_NUMBERS.put("+nan.0", Double.NaN);
    SPECIAL_NUMBERS.put("-nan.0", Double.NaN);
    SPECIAL_NUMBERS.put("+inf.0", Double.POSITIVE_INFINITY);
    SPECIAL_NUMBERS.put("-inf.0", Double.NEGATIVE_INFINITY);
  }

  private static final Map<Character, Integer> NAMED_RADICES = new HashMap<>();
  static {
    NAMED_RADICES.put('b', 2);
    NAMED_RADICES.put('B', 2);
    NAMED_RADICES.put('o', 8);
    NAMED_RADICES.put('O', 8);
    NAMED_RADICES.put('d', 10);
    NAMED_RADICES.put('D', 10);
    NAMED_RADICES.put('x', 16);
    NAMED_RADICES.put('X', 16);
  }

  private static final Map<Integer, BigDecimal> BIG_DECIMAL_RADICES = new HashMap<>();
  static {
    for (int r = 2; r <= 16; r++) {
      BIG_DECIMAL_RADICES.put(r, new BigDecimal(r));
    }
  }

  public static int getRadixByChar(char radixChar) {
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
    RADIX_CHARS.put(3,  "#+-.012");
    RADIX_CHARS.put(4,  "#+-.0123");
    RADIX_CHARS.put(5,  "#+-.01234");
    RADIX_CHARS.put(6,  "#+-.012345");
    RADIX_CHARS.put(7,  "#+-.0123456");
    RADIX_CHARS.put(8,  "#+-.01234567");
    RADIX_CHARS.put(9,  "#+-.012345678");
    RADIX_CHARS.put(10, "#+-.0123456789");
    RADIX_CHARS.put(11, "#+-.0123456789aA");
    RADIX_CHARS.put(12, "#+-.0123456789abAB");
    RADIX_CHARS.put(13, "#+-.0123456789abcABC");
    RADIX_CHARS.put(14, "#+-.0123456789abcdABCD");
    RADIX_CHARS.put(15, "#+-.0123456789abcdeABCDE");
    RADIX_CHARS.put(16, "#+-.0123456789abcdefABCDEF");
  }

  /* Check if digit is valid for a number in a specific radix */
  public static boolean isValidForRadix(char c, int radix) {
    String s = RADIX_CHARS.get(radix);
    if (s == null) {
      throw new IllegalSyntaxException("Bad radix: " + radix);
    }
    return s.indexOf(c) > -1;
  }

  /**
   * Coerce to DECIMAL64 context if one of the numbers has non-zero scale
   */
  public static MathContext getMathContext(BigDecimal first, BigDecimal second) {
    if (first.scale() > 0 || second.scale() > 0) {
      return MathContext.DECIMAL64;
    }
    return MathContext.UNLIMITED;
  }

  /**
   * Rolls back to DEFAULT_CONTEXT if result cannot be represented with UNLIMITED precision
   */
  // FIXME Performance
  public static BigDecimal safeBigDecimalDivision(BigDecimal num, BigDecimal den) {
    try {
      return num.divide(den, NumberUtils.getMathContext(num, den));
    } catch (ArithmeticException e) {
      return num.divide(den, NumberUtils.DEFAULT_CONTEXT);
    }
  }

  /* Check if string represents a valid number and process it */
  public static Object preProcessNumber(String number, Character exactness, int radix) throws ParseException {
    if (number.indexOf('.') != number.lastIndexOf('.')) {
      throw new IllegalSyntaxException("Multiple decimal points: " + number);
    }
    boolean hasBadSignPos = (number.lastIndexOf('+') > 0) || (number.lastIndexOf('-') > 0);
    /* Validate all digits */
    boolean allDigitsAreValid = true;
    boolean hasAtLeastOneDigit = false;
    for (char c : number.toCharArray()) {
      /* Check if char is valid for this radix AND
       * that we don't have # before digits
       */
      if (c != '/' && !isValidForRadix(c, radix) || (c == '#' && !hasAtLeastOneDigit)) {
        allDigitsAreValid = false;
        break;
      }
      /* Check if we have a digit char */
      if ("#+-.".indexOf(c) == -1) {
        hasAtLeastOneDigit = true;
      }
    }

    boolean validHashChars = true;
    if (hasAtLeastOneDigit && number.indexOf('#') > -1) {
      if (HASH_PATTERN.matcher(number).matches()) {
        number = number.replaceAll("#", "0");
        if (exactness == null) {
          exactness = 'i';
        }
      } else {
        validHashChars = false;
      }
    }
    // TODO Exponent

    /* Default exactness for various number types */
    boolean isRational = (number.indexOf('/') > -1);
    boolean isIntegral = (number.indexOf('.') < 0);
    if (exactness == null) {
      if (isRational || isIntegral) {
        exactness = 'e';
      } else {
        exactness = 'i';
      }
    }

    /* Check if it is a rational number */
    boolean validRational = false;
    if (number.indexOf('/') > -1) {
      isRational = true;
      if (number.indexOf('/') == number.lastIndexOf('/')) {
        validRational = true;
      }
      if (number.indexOf('.') > -1) {
        validRational = false;
      }
    }

    if (hasBadSignPos || !allDigitsAreValid || !validHashChars || !hasAtLeastOneDigit || (isRational && !validRational)) {
      /* Not a number! */
      return new SCMSymbol(number);
    }
    /* Drop + sign if exists */
    if (number.charAt(0) == '+') {
      number = number.substring(1);
    }

    int hasSign = (number.charAt(0) == '-') ? 1 : 0;
    if (isRational) {
      String numerator = number.substring(0, number.indexOf('/'));
      String denominator = number.substring(number.indexOf('/') + 1);

      Integer threshold = RADIX_THRESHOLDS.get(radix);
      boolean useBigNum = (numerator.length() > (threshold + hasSign)) ||
                          (denominator.length() > (threshold + hasSign));
      return processRationalNumber(numerator, denominator, radix, exactness, useBigNum);
    }

    Integer threshold = RADIX_THRESHOLDS.get(radix);
    boolean useBigNum = (number.length() > (threshold + hasSign));
    return processNumber(number, radix, exactness, useBigNum);
  }

  /* Parse string into a number */
  private static Number processNumber(String number, Integer r, char exactness, boolean useBigNum) {
    int dot = number.indexOf('.');
    if (useBigNum) {
      if (dot > -1) {
        /* Remove dot */
        number = number.replace(".", "");
        BigInteger bigInteger = new BigInteger(number, r);
        BigDecimal result = new BigDecimal(bigInteger)
          .divide(BIG_DECIMAL_RADICES.get(r).pow(number.length() - dot), MathContext.UNLIMITED);
        if (result.stripTrailingZeros().scale() == 0) {
          result = result.setScale(1, BigDecimal.ROUND_HALF_EVEN);
        }
        return processExactness(result, exactness);
      } else {
        return processExactness(new BigDecimal(new BigInteger(number, r)), exactness);
      }
    }
    if (dot > -1) {
      if (r == 10) {
        return processExactness(Double.parseDouble(number), exactness);
      } else {
        /* Remove dot */
        number = number.replace(".", "");
        return processExactness(parseLong(number, r) / Math.pow(r.doubleValue(), number.length() - dot), exactness);
      }
    }
    return processExactness(Long.parseLong(number, r), exactness);
  }

  private static Number processExactness(Number number, char exactness) {
    if (exactness == 'e') {
      /* For some reason (optimization?), Racket's Reader does not convert into exact numbers 'properly':
       *
       * #e2.3 returns 23/10
       * but
       * (inexact->exact 2.3) returns 2589569785738035/1125899906842624
       *
       * Guile returns 2589569785738035/1125899906842624 in both cases.
       */
      if (isExact(number)) {
        return number;
      } else {
        /* Guile version */
        return toExact(number);

        /* Racket version (non-optimized) */
        // FIXME does not work because number.toString() may format number to Scientific notaion
//        String s = number.toString();
//        int dot = s.indexOf('.');
//        if (dot > -1) {
//          /* Remove dot */
//          s = s.replace(".", "");
//          return new SCMBigRational(new BigInteger(s), BigInteger.TEN.pow(s.length() - dot));
//        }
//        return number;
      }
    }
    if (exactness == 'i') {
      if (number instanceof Long) {
        return number.doubleValue();
      }
      if (number instanceof BigDecimal) {
        int scale = Math.max(1, ((BigDecimal) number).scale());
        return ((BigDecimal) number).setScale(scale);
      }
    }
    /* Other numbers are inexact by default, nothing to do */
    return number;
  }

  /* Parse string into a rational number */
  private static Number processRationalNumber(String numerator, String denominator, Integer r, char exactness,
    boolean useBigNum) {

    Number num = processNumber(numerator, r, 'e', useBigNum);
    Number den = processNumber(denominator, r, 'e', useBigNum);
    SCMBigRational number = new SCMBigRational(new BigInteger(num.toString()), new BigInteger(den.toString()));
    if (exactness == 'i') {
      return toInexact(number);
    }
    return number;
  }

  public static boolean isNumber(Object o) {
    return o instanceof Number;
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
    return false;
  }

  public static boolean isInexact(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Long || o instanceof SCMBigRational || o instanceof Integer || o instanceof BigInteger) {
      return false;
    }
    if (o instanceof BigDecimal) {
      return ((BigDecimal)o).scale() != 0;
    }
    return true;
  }

  public static boolean isRational(Object o) {
    if (!(o instanceof Number)) {
      return false;
    }
    if (o instanceof Double) {
      return !Double.isInfinite((Double) o) && !Double.isNaN((Double) o);
    } else if (o instanceof Float) {
      return !Float.isInfinite((Float) o) && !Float.isNaN((Float) o);
    } else {
      return true;
    }
  }

  public static Number numerator(Object o) {
    if (!isRational(o)) {
      throw new WrongTypeException("Rational", o);
    }
    boolean isExact = isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = toExact(o);
    }
    if (exact instanceof SCMBigRational) {
      BigDecimal result = new BigDecimal(((SCMBigRational) exact).getNumerator());
      if (!isExact) {
        return result.setScale(1);
      }
      return result;
    }
    return exact;
  }

  public static Number denominator(Object o) {
    if (!isRational(o)) {
      throw new WrongTypeException("Rational", o);
    }
    Number exact;
    boolean isExact = isExact(o);
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = toExact(o);
    }
    if (exact instanceof SCMBigRational) {
      BigDecimal result = new BigDecimal(((SCMBigRational) exact).getDenominator());
      if (!isExact) {
        return result.setScale(1);
      }
      return result;
    }
    if (exact instanceof Long || exact instanceof Integer) {
      return 1L;
    }
    if (exact instanceof Double || exact instanceof Float) {
      return 1d;
    }
    if (exact instanceof BigInteger) {
      return BigInteger.ONE;
    }
    if (exact instanceof BigDecimal) {
      if (((BigDecimal) exact).scale() == 0) {
        return BigDecimal.ONE;
      } else {
        return BigDecimal.ONE.setScale(1);
      }
    }
    return 1L;
  }

  public static Number toInexact(Object o) {
    if (!isNumber(o)) {
      throw new WrongTypeException("Number", o);
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).toBigDecimalInexact();
    }
    if (o instanceof BigDecimal) {
      int scale = Math.max(1, ((BigDecimal)o).scale());
      return ((BigDecimal)o).setScale(scale);
    }
    return ((Number)o).doubleValue();
  }

  public static Number toExact(Object o) {
    if (!isNumber(o)) {
      throw new WrongTypeException("Number", o);
    }
    if (o instanceof Float && (Float.isInfinite((Float) o) || Float.isNaN((Float) o))) {
      throw new ArithmeticException("No exact representation");
    }
    if (o instanceof Double) {
      if ((Double.isInfinite((Double) o) || Double.isNaN((Double) o))) {
        throw new ArithmeticException("No exact representation");
      }
      // FIXME There is no need to always call this method!
      return doubleToExact((Double) o);
    }
    if (o instanceof BigDecimal) {
      int scale = ((BigDecimal) o).scale();
      if (scale == 0) {
        return (BigDecimal)o;
      } else {
        return new SCMBigRational(((BigDecimal) o).movePointRight(scale).toBigInteger(), BigInteger.TEN.pow(scale));
      }
    }
    return (Number) o;
  }

  private static Number doubleToExact(Double number) {
    long bits = Double.doubleToLongBits(number);
    long sign = bits >>> 63;
    long exponent = ((bits >>> 52) ^ (sign << 11)) - 1023;
    long fraction = bits << 12;
    long a = 1L;
    long b = 1L;
    for (int i = 63; i >= 12; i--) {
      a = a * 2 + ((fraction >>> i) & 1);
      b *= 2;
    }
    if (exponent > 0) {
      a *= 1 << exponent;
    } else {
      b *= 1 << -exponent;
    }
    if (sign == 1) {
      a *= -1;
    }
    return new SCMBigRational(BigInteger.valueOf(a), BigInteger.valueOf(b));
  }
}
