package com.nedap.university.erik.dominoeffect;

import java.util.Collections;

/** Created by erik.huizinga on 13-10-17 */
public class Tools {

  public static String pad(Integer integer, int maxPips) {
    String s = integer.toString();
    int n = 2 + ((int) Math.floor(Math.log10(2 * Bone.totalNumber(maxPips))));
    int m = n - s.length();
    return s + String.join("", Collections.nCopies(m, " "));
  }
}
