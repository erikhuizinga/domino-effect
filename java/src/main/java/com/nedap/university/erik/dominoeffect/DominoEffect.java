package com.nedap.university.erik.dominoeffect;

import com.nedap.university.erik.dominoeffect.Puzzles.Assignment1;

/** Created by erik.huizinga on 8-10-17 */
public class DominoEffect {
  public static void main(String[] args) {
    Puzzle puzzle = new Assignment1();
    System.out.println(puzzle.print(puzzle.getMaxPips()));
  }
}
