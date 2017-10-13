package com.nedap.university.erik.dominoeffect;

import java.util.stream.IntStream;

/** Created by erik.huizinga on 13-10-17 */
public class Bone {

  public static int totalNumber(int maxPips) {
    return IntStream.rangeClosed(1, maxPips + 1).sum();
  }
}
