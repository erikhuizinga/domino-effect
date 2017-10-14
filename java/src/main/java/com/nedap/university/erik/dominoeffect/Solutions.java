package com.nedap.university.erik.dominoeffect;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Solutions {
  private static Set<Position> positions6 = Position.initialSetOf(6);
  public static Solution solution1_1 =
      new Solution(
          positions6,
          List.of(
              28, 28, 14, 7, 17, 17, 11, 11, 10, 10, 14, 7, 2, 2, 21, 23, 8, 4, 16, 25, 25, 13, 21,
              23, 8, 4, 16, 15, 15, 13, 9, 9, 12, 12, 22, 22, 5, 5, 26, 26, 27, 24, 24, 3, 3, 18, 1,
              19, 27, 6, 6, 20, 20, 18, 1, 19));

  public static String print(Collection<Solution> solutions) {
    return String.join("\n\n", solutions.stream().map(Puzzle::print).collect(Collectors.toList()));
  }
}
