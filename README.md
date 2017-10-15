```
        D O M I N O
 ___                    ___
|o o|   E F F E C T    |o o|
|o_o| ___ ___  ___ ___ |o_o|
|o  ||o  |ooo||ooo|o o||o o|
|__o||__o|ooo||ooo|o_o||o_o|
```

> *Solve the Domino Effect challenge in under 100 milliseconds!*


Developed by [*Erik Huizinga*](https://github.com/erikhuizinga), supervised by [*Teun van Hemert*](https://github.com/teunvanhemert) and [*Steven Rekké*](https://github.com/srekke).

## Introduction

*Domino Effect* is a puzzle involving dominoes.
A field of numbers needs to be filled with domino pieces (bones) such that the number of dots (pips) on the bones correspond to the numbers on the puzzle field.
This project aims to solve this puzzle with both a functional implementation in Haskell and a not-so-functional implementation in Java.

## Report

[See the wiki.](https://www.github.com/erikhuizinga/domino-effect/wiki/Report)

## Running Domino Effect

Get the [latest release](https://github.com/erikhuizinga/domino-effect/releases/latest), download the binaries and run them:

### Haskell

| Platform | Shell | Command |
| :------------- | :------------- | :------------- |
| macOS | `bash` | `./domino-effect` |
| Windows | `powershell` | `.\domino-effect` |

#### Running GHCi

With the Haskell Stack installed (see below), run `stack ghci`. Here, use one of the following functions:

| Function | Description |
| :------------- | :------------- |
| `main` | Runs the same as the binary, i.e., it solves a random puzzle. |
| `scriptDominoEffect` | Solves a fixed puzzle with 24 solutions. |
| `dominoEffect maxPips` | Solves a random puzzle with `maxPips` as the maximum number of pips on a bone. |
| `funDominoEffect puzzle maxPips` | Solves the given `puzzle` with `maxPips` as the maximum number of pips on a bone. |

`Puzzle` has type `[Pips]`, with `Pips` being `Int`. So it is just a list of numbers of pips in the puzzle. They are oriented from top left from left to right to bottom right. An example syntax would be:

```haskell
funDominoEffect [0,0,0,1,1,1] 1
```

### Java

With JRE 9 installed, run `java -jar domino-effect.jar`.

The Java solves a puzzle from the assignment, but has no other user interaction.

## Building Domino Effect

### Haskell preparation

[The Haskell Tool Stack](https://haskellstack.org) was used to develop this project. It is a Haskell dependency management system, comparable to the well-known Gradle. The following instructions assume the Haskell Stack is not installed or up to date.

#### macOS w/ [homebrew](https://brew.sh)

In `bash`, in the root of this repo:

```bash
brew install haskell-stack
stack setup
# stack init # may or may not be necessary
stack build
stack exec domino-effect
```

#### Windows w/ [chocolatey](https://chocolatey.org/)

In `powershell` with admin rights:

```powershell
choco install haskell-stack
```

Then, in the root of this repo:

```powershell
stack setup
# stack init # may or may not be necessary
stack build
stack exec domino-effect
```

### IntelliJ IDEA

Prerequisites:

1. Haskell Stack
1. IntelliJ IDEA w/ IntelliJ-Haskell plugin (restart after install) & Gradle v4.2.1
1. JDK 9

Steps to import the project in IntelliJ IDEA:

1. Use Git to `git clone` the repo or download the zip from GitHub and extract it.
1. Import the repo root as a project in IntelliJ IDEA.
1. Add run configurations for both Java and Haskell:
   1. Run > Edit Configurations... > + > Haskell Stack > Haskell Stack Runner > name the configuration > remove the Before launch Build step > OK.
   1. Open `java/src/DominoEffect.java`, right click the `main` method and select Run 'Java' > Save the newly created run configuration. Alternatively, use the Gradle side menu to run tasks.
