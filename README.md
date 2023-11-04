# Tom-and-Jerry

## Contribution
Implementation is distributed evenly and is done via VScode's LiveShare extension.

## Description
This is the code for our final project of the course CSE301 *Functional Programming* at Ecole Polytechnique.
The BTW directory contains the reference code provided by the course for the project, and the simple tree game **Binary Tree World**.
The TAJ directory contains our adaptation and implementation of our game **Tom and Jerry**.

In ghci, to play, load the ```TomAndJerry.hs``` and type ```main```. You would play Tom the cat who tries to catch Jerry before he escapes the trees.
Read the instructions. Good luck!

```
ghci> :l TomAndJerry.hs
[1 of 5] Compiling Bin              ( Bin.hs, interpreted )
[2 of 5] Compiling Cmd              ( Cmd.hs, interpreted )
[3 of 5] Compiling Parser           ( Parser.hs, interpreted )
ghci> main
Welcome to Tom and Jerry.
```

There are four mouse tactics for you to choose from (adjust them at the second last line of the file ```TomAndJerry.hs```):
- ```standStillJerry``` : The boring do-nothing tactic, for debugging purposes mainly
- ```moveRandomJerry``` : A random tactic
- ```bestMoveJerry```   : A greedy tactic for the mouse
- ```subOptimalJerry``` : A more natural and realistic behavior

Also, feel free to change the depth of the tree using parameter ```layer``` should you feel the need to.

Here is the [link](https://noamz.org/teaching/CSE301/Project/README.html) of the offical final project assignment.
