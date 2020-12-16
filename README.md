# sudoku
This is an implementation of the popular game in Common Lisp

The primary objective of this implementation is speed (as it usually is).

At the moment the solution only works for the simplest of puzzles. The next step is to get it to work correctly for all puzzles, and finally to make it fast for all puzzles, including the hardest 'Evit' puzzles out there.


## Running the code
Currently the only way to input a Sudoku puzzle grid is to enter it at the REPL.
An unsolved puzzle may be read line-by-line from the command line, or entered enmass as a 2D array.


## Examples

### Entering the Sudoku grid at the REPL
CL-USER> (sdk/read-grid)

0 0 3 7 0 5 2 0 8 

0 0 0 0 8 0 0 0 4 

0 2 0 4 0 3 5 9 0 

0 0 2 0 0 0 4 0 6 

0 3 0 9 0 6 0 1 0 

7 0 8 0 0 0 9 0 0 

0 7 6 8 0 4 0 2 0 

2 0 0 0 9 0 0 0 0 

4 0 9 3 0 1 6 0 0 

### Entering the whole grid at once
CL-USER> (sdk/solve

#2A(((0) (0) (3) (7) (0) (5) (2) (0) (8))

    ((0) (0) (0) (0) (8) (0) (0) (0) (4))
    
    ((0) (2) (0) (4) (0) (3) (5) (9) (0))
    
    ((0) (0) (2) (0) (0) (0) (4) (0) (6))
    
    ((0) (3) (0) (9) (0) (6) (0) (1) (0))
    
    ((7) (0) (8) (0) (0) (0) (9) (0) (0))
    
    ((0) (7) (6) (8) (0) (4) (0) (2) (0))
    
    ((2) (0) (0) (0) (9) (0) (0) (0) (0))
    
    ((4) (0) (9) (0) (0) (1) (6) (0) (0))
    )
)
