package com.nedap.university.erik.dominoeffect;

import java.util.List;

/** Created by erik.huizinga on 13-10-17 */
public class Puzzles {

  public static class Assignment1 extends Puzzle {

    public static int maxPips = 6;

    private Assignment1(List<Position> ignored1, List<Integer> ignored2) {
      this();
    }

    public Assignment1() {
      super(
          Position.initialSetOf(Assignment1.maxPips),
          List.of(
              6, 6, 2, 6, 5, 2, 4, 1, 1, 3, 2, 0, 1, 0, 3, 4, 1, 3, 2, 4, 6, 6, 5, 4, 1, 0, 4, 3, 2,
              1, 1, 2, 5, 1, 3, 6, 0, 4, 5, 5, 5, 5, 4, 0, 2, 6, 0, 3, 6, 0, 5, 3, 4, 2, 0, 3));
    }
  }
}
