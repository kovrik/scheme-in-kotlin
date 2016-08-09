package core.utils;

import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

public class NumberUtils {

  private NumberUtils() {}

  private static final Map<Character, Integer> NAMED_RADICES = new HashMap<>();
  static {
    NAMED_RADICES.put('b', 2);
    NAMED_RADICES.put('o', 8);
    NAMED_RADICES.put('d', 10);
    NAMED_RADICES.put('x', 16);
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
    RADIX_CHARS.put(2,  "+-.01");
    RADIX_CHARS.put(3,  "+-.012");
    RADIX_CHARS.put(4,  "+-.0123");
    RADIX_CHARS.put(5,  "+-.01234");
    RADIX_CHARS.put(6,  "+-.012345");
    RADIX_CHARS.put(7,  "+-.0123456");
    RADIX_CHARS.put(8,  "+-.01234567");
    RADIX_CHARS.put(9,  "+-.012345678");
    RADIX_CHARS.put(10, "+-.0123456789");
    RADIX_CHARS.put(11, "+-.0123456789aA");
    RADIX_CHARS.put(12, "+-.0123456789abAB");
    RADIX_CHARS.put(13, "+-.0123456789abcABC");
    RADIX_CHARS.put(14, "+-.0123456789abcdABCD");
    RADIX_CHARS.put(15, "+-.0123456789abcdeABCDE");
    RADIX_CHARS.put(16, "+-.0123456789abcdefABCDEF");
  }

  /* Check if digit is valid for a number in a specific radix */
  public static boolean isValidForRadix(char c, int radix) {
    String s = RADIX_CHARS.get(radix);
    if (s == null) {
      throw new IllegalSyntaxException("Bad radix: " + radix);
    }
    return s.indexOf(c) > -1;
  }

  /* Check if string represents a valid number and process it */
  public static Object preProcessNumber(String number, char exactness, int radix) throws ParseException {
    boolean hasTwoDots = number.indexOf('.') != number.lastIndexOf('.');
    boolean isSignCharOnly = (number.length() == 1) && (number.charAt(0) == '+' || number.charAt(0) == '-');
    boolean hasBadSignPos = (number.lastIndexOf('+') > 0) || (number.lastIndexOf('-') > 0);
    /* Validate all digits */
    boolean allDigitsAreValid = true;
    for (char c : number.toCharArray()) {
      if (!isValidForRadix(c, radix)) {
        allDigitsAreValid = false;
        break;
      }
    }
    if (hasTwoDots || isSignCharOnly || hasBadSignPos || !allDigitsAreValid) {
      /* Not a number! */
      return new SCMSymbol(number);
    }
    /* Drop + sign if exists */
    if (number.charAt(0) == '+') {
      number = number.substring(1);
    }
    /* Check exactness */
    // TODO Exactness

    int hasSign = (number.charAt(0) == '-') ? 1 : 0;
    Integer threshold = RADIX_THRESHOLDS.get(radix);
    boolean useBigNum = (number.length() > (threshold + hasSign));
    return processNumber(number, radix, exactness, useBigNum);
  }

  /* Parse string into a number */
  private static Number processNumber(String number, Integer r, char exactness, boolean useBigNum) {
    int hasSign = (number.charAt(0) == '-') ? 1 : 0;
    int dot = number.indexOf('.');
    if (useBigNum) {
      if (dot > -1) {
        /* Remove dot */
        number = number.replace(".", "");
        BigInteger bigInteger = new BigInteger(number, r);
        return new BigDecimal(bigInteger).divide(new BigDecimal(r).pow(number.length() - hasSign - dot), MathContext.DECIMAL32);
      }
      BigInteger bigInteger = new BigInteger(number, r);
      return new BigDecimal(bigInteger);
    }
    if (dot > -1) {
      if (r == 10) {
        return Double.parseDouble(number);
      } else {
        /* Remove dot */
        number = number.replace(".", "");
        return Long.parseLong(number, r) / Math.pow(r.doubleValue(), number.length() - hasSign - dot);
      }
    }
    return Long.parseLong(number, r);
  }
}
