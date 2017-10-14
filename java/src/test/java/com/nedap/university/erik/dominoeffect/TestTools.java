package com.nedap.university.erik.dominoeffect;

import static org.junit.jupiter.api.Assertions.fail;

import java.util.Collection;

/** Created by erik.huizinga on 13-10-17 */
public class TestTools {

  /** Why does {@code set1.containsAll(set2) && set2.containsAll(set1)} not work? */
  public static <T> void checkEquivalence(Collection<T> expectedSet, Collection<T> actualSet) {
    boolean ok = false;
    for (T expected : expectedSet) {
      for (T actual : actualSet) {
        if (expected.equals(actual)) {
          ok = true;
          break;
        }
      }
      if (ok) {
        break;
      }
    }
    if (!ok) {
      fail("An expected was not found");
    }
  }
}
