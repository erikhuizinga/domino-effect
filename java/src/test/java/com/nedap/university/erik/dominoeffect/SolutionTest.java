package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Solution.DEFAULT;
import static com.nedap.university.erik.dominoeffect.TestData.pos0;
import static com.nedap.university.erik.dominoeffect.TestData.puzzle;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class SolutionTest {

  @BeforeEach
  void setUp() {
    TestData.reset();
  }

  @Test
  void isSolved() {
    assertFalse(new Solution(List.of(pos0), List.of(DEFAULT)).isSolved());
    assertTrue(new Solution(Collections.emptyList(), Collections.emptyList()).isSolved());
  }

  @Test
  void initialize() {
    Collection<Integer> initialSolution = Solution.initializeFor(puzzle).values();
    assertTrue(Set.of(DEFAULT).containsAll(initialSolution));
    assertTrue(initialSolution.containsAll(Set.of(DEFAULT)));
  }
}
