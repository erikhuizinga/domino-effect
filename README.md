```
        D O M I N O
 ___                    ___
|o o|   E F F E C T    |o o|
|o_o| ___ ___  ___ ___ |o_o|
|o  ||o  |ooo||ooo|o o||o o|
|__o||__o|ooo||ooo|o_o||o_o|
```


Developed by [*Erik Huizinga*](https://github.com/erikhuizinga), supervised by [*Teun van Hemert*](https://github.com/teunvanhemert) and [*Steven Rekké*](https://github.com/srekke).

## Introduction

*Domino Effect* is a puzzle involving dominoes.
A field of numbers needs to be filled with domino pieces (bones) such that the number of dots (pips) on the bones correspond to the numbers on the puzzle field.
This project aims to solve this puzzle with both a functional implementation in Haskell and a not-so-functional implementation in Java.

## Running Domino Effect

*To be completed...*

### Haskell

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

In `powershell`, in the root of this repo:

```powershell
choco install haskell-stack
stack setup
# stack init # may or may not be necessary
stack build
stack exec domino-effect
```

### Java

From the root of this repo:

```powershell
cd java\src
javac DominoEffect.java -d ..\out
cd ..\out
java DominoEffect
```

## Building Domino Effect

Prerequisites:

1. Git
1. Haskell Stack
1. IntelliJ IDEA w/ IntelliJ-Haskell plugin (restart after install)
1. JDK 9

Steps to import the project in IntelliJ IDEA:

1. `git clone` the repo.
1. Import the repo root as a project in IntelliJ IDEA.
1. Add run configurations for both Java and Haskell:
   1. Run > Edit Configurations... > + > Haskell Stack > Haskell Stack Runner > name the configuration > remove the Before launch Build step > OK.
   1. Open `java/src/DominoEffect.java`, right click the `main` method and select Run 'Java' > Save the newly created run configuration.
