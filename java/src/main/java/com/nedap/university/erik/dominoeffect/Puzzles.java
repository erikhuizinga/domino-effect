package com.nedap.university.erik.dominoeffect;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Puzzles {

  private static final List<Integer> assignment1List =
      List.of(
          6, 6, 2, 6, 5, 2, 4, 1, 1, 3, 2, 0, 1, 0, 3, 4, 1, 3, 2, 4, 6, 6, 5, 4, 1, 0, 4, 3, 2, 1,
          1, 2, 5, 1, 3, 6, 0, 4, 5, 5, 5, 5, 4, 0, 2, 6, 0, 3, 6, 0, 5, 3, 4, 2, 0, 3);
  public static Puzzle assignment1 =
      new Puzzle(Position.initialSetOf(Collections.max(assignment1List)), assignment1List);

  public static String print(Collection<Puzzle> puzzles) {
    return String.join("\n\n", puzzles.stream().map(Puzzle::print).collect(Collectors.toList()));
  }
}
