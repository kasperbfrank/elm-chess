## MVP
- [x] Store pieces in Dict Position Piece
- [x] Make recursive function to figure return possible moves.
Something that takes a `MoveCount` describing how 
many moves are possible in any given direction, and a `(x, y)`
describing distance of a single move a piece, i.e. Horsie would be
`(1, 2)`, Queen would be `(1, 1)`
- [x] Implement "castling" for King
- [x] Make sure player cannot put own King in check
- [ ] If pawn reaches other side of the board, make it possible to trade for other piece
- [x] Implement victory state + play again
- [x] Make it more visible when a move destroys enemy unit
- [x] Implement [en passant rule](https://www.chess.com/terms/en-passant)
  - [x] Make move history (this also eliminates the need for moveCount)
  - [x] Add startSquare to PieceDetails
  - [x] Check if last move was move that allows for en passant

## Ideas
- [ ] Hover effects for moves
- [ ] Correct row / col naming, e.g. (A, 1)
- [ ] Fn for translating e.g. 1 -> A
- [ ] Drag pieces
- [ ] Music, Themes
- [ ] Disco chess! Where board changes color like disco dance floor
- [ ] Mortal Kombat style animations and sound effects 
- [ ] Messages with cool animation for check and checkmate
- [ ] Support for playing by keyboard, typing e.g. a1 -> a2
- [ ] Play against others, using websockets and server
- [ ] Lobby with games
- [ ] Practice mode where you can just play both sides yourself
  - [ ] And can undo in move history
  - [ ] "Time travel" to move back in move history
- [ ] Skins for pieces
- [ ] Logging of games to Humio