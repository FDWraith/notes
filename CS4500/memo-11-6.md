### Fish Game Milestone 6 Memo -- Referee Component

#### Date and Time

November 6th, 3:40 PM

#### Presenters

Andrew J. Leung 

Fiona M. Gridley

#### Panelists

Head: Nicholas W. Fletcher

Assistant: Paul Rhee

Secretary: Kevin Zhang 



#### Problems Discovered (And Possible Solutions)

- The Referee should be able to create interesting boards (such as boards with holes or different fish count).
- `runPlacementRounds` doesn't account for ended game when every player cheats during placing penguins; the while loop checks for `gameIsMovingGame`, but not for an ended game.
- The Referee should be able to handle non-cooperating players, such as players that take a long time to play, or refuse to provide a move.
  - Include a timer and then add players to `failingPlayers` after the timer runs out.
- Creating a `GameTree` each time to check a valid move might be expensive, because the tree still has to generate other nodes.
- Abstract over `runMovementRounds` and `runPlacementRounds` to reduce code duplication.
- When handing players the state, make a deep copy to prevent modification.
-  `cheatingPlayers` and `failingPlayers` are merged into `kickedPlayers` in the `GameDebrief`. Have one field for each in the `GameDebrief` so that receivers of the `GameDebrief` will also know.
- Suggestion: rename `isMovementLegal` to `makeMoveIfLegal`, because it should imply that it *actually* makes the move, and is not simply a check.

