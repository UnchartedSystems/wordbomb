## Loose Plan

### Define and Iterate on the Rules

- vertically adjacent linked boxes share the same letter
- vertically adjacent unlinked boxes cannot share the same letter
- Some letters are locked by default
- Fill all words correctly to win

### Define a common data structure to represent games

An array of alternating

- Pair of index of letter in word & actual locked letter
- Array of positions of vertical links

[[4, P], [0, 2], [1, N], [3, 4], [0, F], [1, 2], [3, C], [0, 4], [2, A]] 

### Create a Terminal Player

- Create full Word list
- Convert a puzzle input into a game state
- Represent the Game State
- Accept full input for the game input (all 5 words)
- Check input against rules, tally rule violations
  - If Violations: Represent Rule violations
  - If no violations: Represent Victory

## Create a Generator

tbd

## Create a Solver

tbd

