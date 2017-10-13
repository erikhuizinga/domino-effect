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
  void compareTo() {
    assertTrue(bone1.compareTo(bone2) < 0);
    assertTrue(bone2.compareTo(bone2) == 0);
    assertTrue(bone3.compareTo(bone2) > 0);
  }
}
