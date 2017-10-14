package com.nedap.university.erik.dominoeffect;

import java.util.Set;

/** Created by erik.huizinga on 8-10-17 */
public class DominoEffect {
  public static void main(String[] args) {
    Puzzle puzzle = Puzzles.assignment1;
    int maxPips = puzzle.getMaxPips();

    System.out.println(welcome());
    System.out.println();

    System.out.println("Bones:");
    Set<Bone> bones = Bone.initialSetOf(maxPips);
    System.out.println(Bone.print(bones, maxPips));
    System.out.println();

    System.out.println("Puzzle:");
    System.out.println(puzzle.print());
    System.out.println();

    System.out.print("Solving... ");
    Set<Solution> solutions =
        puzzle.solve(Position.initialSetOf(maxPips), bones, Solution.initializeFor(puzzle));
    System.out.println(solutions.size() + " solutions found!");
    System.out.println();

    System.out.println("Solutions:");
    System.out.println(Solutions.print(solutions));
  }

  private static String welcome() {
    return "\n        D O M I N O\n"
        + " ___                    ___\n"
        + "|o o|   E F F E C T    |o o|\n"
        + "|o_o| ___ ___  ___ ___ |o_o|\n"
        + "|o  ||o  |ooo||ooo|o o||o o|\n"
        + "|__o||__o|ooo||ooo|o_o||o_o|\n";
  }
}
