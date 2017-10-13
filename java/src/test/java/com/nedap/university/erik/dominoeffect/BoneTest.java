package com.nedap.university.erik.dominoeffect;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class BoneTest {

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
}
