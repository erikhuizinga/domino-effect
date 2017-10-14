package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Solution.DEFAULT;
import static com.nedap.university.erik.dominoeffect.TestData.pos0;
import static com.nedap.university.erik.dominoeffect.TestData.puzzle1;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Assertions;
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
    Collection<Integer> initialSolution = Solution.initializeFor(puzzle1).values();
    assertTrue(Set.of(DEFAULT).containsAll(initialSolution));
    assertTrue(initialSolution.containsAll(Set.of(DEFAULT)));
  }

  @Test
  void print() {
    String print = Solutions.solution1_1.print();
    System.out.println(print);
    Assertions.assertEquals(
        "28 28 14 7  17 17 11 11 \n"
            + "10 10 14 7  2  2  21 23 \n"
            + "8  4  16 25 25 13 21 23 \n"
            + "8  4  16 15 15 13 9  9  \n"
            + "12 12 22 22 5  5  26 26 \n"
            + "27 24 24 3  3  18 1  19 \n"
            + "27 6  6  20 20 18 1  19 ",
        print);
    System.out.println();

    String print1 = Solutions.print(List.of(Solutions.solution1_1, Solutions.solution1_1));
    System.out.println(print1);
    Assertions.assertEquals(
        "28 28 14 7  17 17 11 11 \n"
            + "10 10 14 7  2  2  21 23 \n"
            + "8  4  16 25 25 13 21 23 \n"
            + "8  4  16 15 15 13 9  9  \n"
            + "12 12 22 22 5  5  26 26 \n"
            + "27 24 24 3  3  18 1  19 \n"
            + "27 6  6  20 20 18 1  19 \n"
            + "\n"
            + "28 28 14 7  17 17 11 11 \n"
            + "10 10 14 7  2  2  21 23 \n"
            + "8  4  16 25 25 13 21 23 \n"
            + "8  4  16 15 15 13 9  9  \n"
            + "12 12 22 22 5  5  26 26 \n"
            + "27 24 24 3  3  18 1  19 \n"
            + "27 6  6  20 20 18 1  19 ",
        print1);
  }
}
