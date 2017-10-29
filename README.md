# Sudoku Generator in Haskell

## How it's done?

It starts with an empty grid, and creates a valid finished game of sudoku via
backtrack. In this backtrack, the order in which the numbers are tested is
random, so the resulting grid, wont be the same every time.

After the finished game of sudoku is created, it removes a certain amount
of numbers from the grid, creating an not finished but possible to complete,
game of sudoku.

## Build
```
stack clean && stack setup && stack build
```

## Examples:

If for example we want a sudoku grid with just 10 starting number:

```
$ stack exec sudoku-generator-exe 10
+---+---+---+---+---+---+---+---+---+
|   |   | 8 |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   | 6 |   |
+---+---+---+---+---+---+---+---+---+
|   | 2 |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | 2 |   |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | 6 |   | 4 |
+---+---+---+---+---+---+---+---+---+
| 2 |   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   | 7 |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
| 5 |   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | 5 |   |   |
+---+---+---+---+---+---+---+---+---+
```
Or with 45 starting numbers:
```
$ stack exec sudoku-generator-exe 45
+---+---+---+---+---+---+---+---+---+
|   | 6 | 2 | 3 |   | 9 | 4 |   |   |
+---+---+---+---+---+---+---+---+---+
| 3 | 4 | 9 |   | 8 |   | 5 |   | 2 |
+---+---+---+---+---+---+---+---+---+
| 5 |   |   | 6 | 4 |   | 7 |   | 9 |
+---+---+---+---+---+---+---+---+---+
|   |   | 7 |   |   | 3 | 8 | 1 | 4 |
+---+---+---+---+---+---+---+---+---+
|   | 9 |   | 8 | 1 |   | 6 | 2 |   |
+---+---+---+---+---+---+---+---+---+
| 8 |   | 5 |   | 2 |   |   | 9 |   |
+---+---+---+---+---+---+---+---+---+
|   |   | 6 |   | 3 | 5 | 1 | 4 |   |
+---+---+---+---+---+---+---+---+---+
| 9 | 3 | 4 | 1 |   |   | 2 |   | 6 |
+---+---+---+---+---+---+---+---+---+
|   |   | 8 |   | 6 |   | 9 |   | 3 |
+---+---+---+---+---+---+---+---+---+
```
