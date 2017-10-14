package com.nedap.university.erik.dominoeffect;

import java.util.Set;

/** Created by erik.huizinga on 8-10-17 */
public class DominoEffect {
  public static void main(String[] args) {
    Puzzle puzzle = Puzzles.assignment1;
    int maxPips = puzzle.getMaxPips();

    System.out.println("Domino Effect");
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
    System.out.println(Solutions.print(solutions));
    System.out.println();
  }
}
