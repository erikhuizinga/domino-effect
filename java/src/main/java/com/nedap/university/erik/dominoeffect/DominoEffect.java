package com.nedap.university.erik.dominoeffect;

import com.nedap.university.erik.dominoeffect.Puzzles.Assignment1;
import java.util.Set;

/** Created by erik.huizinga on 8-10-17 */
public class DominoEffect {
  public static void main(String[] args) {
    Puzzle puzzle = new Assignment1();
    int maxPips = puzzle.getMaxPips();

    System.out.println("Domino Effect");
    System.out.println();

    System.out.println("Bones:");
    Set<Bone> bones = Bone.initialSetOf(maxPips);
    System.out.println(Bone.print(bones, maxPips));
    System.out.println();

    System.out.println(puzzle.print(maxPips));
  }
}
