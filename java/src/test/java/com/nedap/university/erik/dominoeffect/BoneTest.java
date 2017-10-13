package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.TestData.bone1;
import static com.nedap.university.erik.dominoeffect.TestData.bone2;
import static com.nedap.university.erik.dominoeffect.TestData.bone3;
import static com.nedap.university.erik.dominoeffect.TestData.bones1;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Collection;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class BoneTest {

  @BeforeEach
  void setUp() {
    TestData.reset();
  }

  @Test
  void totalNumber() {
    assertEquals(1, Bone.totalNumber(0));
    assertEquals(3, Bone.totalNumber(1));
    assertEquals(6, Bone.totalNumber(2));
    assertEquals(10, Bone.totalNumber(3));
    assertEquals(15, Bone.totalNumber(4));
    assertEquals(21, Bone.totalNumber(5));
    assertEquals(28, Bone.totalNumber(6));
  }

  @Test
  void initialSetOf() {
    Set<Bone> bones = Bone.initialSetOf(1);
    checkEquality(bones1, bones);
    checkEquality(bones, bones1);
  }

  /** Why does {@code set1.containsAll(set2) && set2.containsAll(set1)} not work? */
  private void checkEquality(Collection<Bone> expectedBones, Collection<Bone> actualBones) {
    boolean ok = false;
    for (Bone expected : expectedBones) {
      for (Bone actual : actualBones) {
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
      fail("Expected Bone not found");
    }
  }

  @Test
  void print() {
    int maxPips = 6;
    Set<Bone> bones = Bone.initialSetOf(maxPips);
    assertEquals(
        "#1  0|0\n"
            + "#2  0|1\n"
            + "#3  0|2\n"
            + "#4  0|3\n"
            + "#5  0|4\n"
            + "#6  0|5\n"
            + "#7  0|6\n"
            + "#8  1|1\n"
            + "#9  1|2\n"
            + "#10 1|3\n"
            + "#11 1|4\n"
            + "#12 1|5\n"
            + "#13 1|6\n"
            + "#14 2|2\n"
            + "#15 2|3\n"
            + "#16 2|4\n"
            + "#17 2|5\n"
            + "#18 2|6\n"
            + "#19 3|3\n"
            + "#20 3|4\n"
            + "#21 3|5\n"
            + "#22 3|6\n"
            + "#23 4|4\n"
            + "#24 4|5\n"
            + "#25 4|6\n"
            + "#26 5|5\n"
            + "#27 5|6\n"
            + "#28 6|6",
        Bone.print(bones, maxPips));
  }

  @Test
  void compareTo() {
    assertTrue(bone1.compareTo(bone2) < 0);
    assertTrue(bone2.compareTo(bone2) == 0);
    assertTrue(bone3.compareTo(bone2) > 0);
  }
}
