#|
   TTTTTTTTTTTT EEEEEEEEEEEE TTTTTTTTTTTT RRRRRRRRRRRR IIIIIIIIIIII SSSSSSSSSSSS
   TTTTTTTTTTTT EEEEEEEEEEEE TTTTTTTTTTTT RRRRRRRRRRRR IIIIIIIIIIII SSSSSSSSSSSS
   .   TTTT     EEEE             TTTT     RRRR    RRRR     IIII     SSSS        
   .   TTTT     EEEE             TTTT     RRRR    RRRR     IIII     SSSS        
   .   TTTT     EEEEEEEE         TTTT     RRRRRRRRRRRR     IIII     SSSSSSSSSSSS
   .   TTTT     EEEEEEEE         TTTT     RRRRRRRRRRRR     IIII     SSSSSSSSSSSS
   .   TTTT     EEEE             TTTT     RRRRRRRR         IIII             SSSS
   .   TTTT     EEEE             TTTT     RRRR  RRRR       IIII             SSSS
   .   TTTT     EEEEEEEEEEEE     TTTT     RRRR    RRRR IIIIIIIIIIII SSSSSSSSSSSS
   .   TTTT     EEEEEEEEEEEE     TTTT     RRRR    RRRR IIIIIIIIIIII SSSSSSSSSSSS
   
   By: Thomas Del Vecchio
|#

include reactors
include image
include image-structs

#============= CONSTANTS =============#
TILE-WIDTH = 40
BOARD-DIMS = {w : 10, h : 20}

BG-COLOR = black
SB-COLOR = white
TILE-COLORS = {
  N : color(255, 255, 255, 0),
  O : yellow,
  I : cyan,
  T : purple,
  L : orange,
  J : medium-blue,
  S : green,
  Z : red,
  H : gray}

#============= DATA TYPES =============#

# Tetris Data Types

type Board = List<List<Tile>>

data Tile:
  | t-N | t-H
  | t-O | t-I
  | t-T
  | t-L | t-J
  | t-S | t-Z
end

data Tetrimino:
  | tetrimino(piece :: Board,
      x :: Number, y :: Number,
      shape :: Tile, rot :: Number)
end

TETRIMINO-PIECES = [list:
  tetrimino([list: # O-Piece
      [list: t-O, t-O],
      [list: t-O, t-O]],
    4, 0, t-O, 0),
  tetrimino([list: # I-Piece
      [list: t-N, t-N, t-N, t-N],
      [list: t-I, t-I, t-I, t-I],
      [list: t-N, t-N, t-N, t-N],
      [list: t-N, t-N, t-N, t-N]],
    3, 0, t-I, 0),
  tetrimino([list: # T-Piece
      [list: t-N, t-T, t-N],
      [list: t-T, t-T, t-T],
      [list: t-N, t-N, t-N]],
    3, 0, t-T, 0),
  tetrimino([list: # L-Piece
      [list: t-N, t-N, t-L],
      [list: t-L, t-L, t-L],
      [list: t-N, t-N, t-N]],
    3, 0, t-L, 0),
  tetrimino([list: # J-Piece
      [list: t-J, t-N, t-N],
      [list: t-J, t-J, t-J],
      [list: t-N, t-N, t-N]],
    3, 0, t-J, 0),
  tetrimino([list: # S-Piece
      [list: t-N, t-S, t-S],
      [list: t-S, t-S, t-N],
      [list: t-N, t-N, t-N]],
    3, 0, t-S, 0),
  tetrimino([list: # Z-Piece
      [list: t-Z, t-Z, t-N],
      [list: t-N, t-Z, t-Z],
      [list: t-N, t-N, t-N]],
    3, 0, t-Z, 0)]

# General Data Types

data Stream<T>:
  | lz-link(first :: T, rest :: ( -> Stream<T>)) 
end

# Game Data Types

data State:
  | game-over(board :: Board,
      pieces :: Stream<Tetrimino>,
      score :: Number)
  | game(board :: Board,
      pieces :: Stream<Tetrimino>,
      curr-piece :: Tetrimino,
      score :: Number)
end






#============= HELPER METHODS =============#
  
# misc

fun illegal-state(state :: State) -> Boolean:
  doc: ```Produces whether a state is legal or not;
       Illegal states include overlapping pieces and
       piece off of board.```
  cases (State) state:
    | game-over(_, _, _) => true
    | game(board, _, piece, _) =>
      for fold3(colide from false,
          rown from range(piece.y, piece.y + 5),
          prow from piece.piece, 
          # Hacky way of dealing with empty space of piece off board
          brow from ([list: empty, empty] + board)
            .drop(num-max(piece.y + 2, 2))):

        colide or
        for fold3(colide2 from colide,
            coln from range(piece.x, piece.x + 5),
            pcol from prow, 
            # Hacky way of fixing quick turn when piece spawns
            bcol from ([list: t-N, t-N] + brow)
              .drop(num-max(piece.x + 2, 0)) + [list: t-N]):

          colide2 or
          ask:
              # Ignore if not actual tile
            | pcol == t-N then: false
              # Off screen horizontally
            | ((coln < 0) or (coln >= BOARD-DIMS.w)) then: true
              # Off screen vertically
            | rown >= BOARD-DIMS.h then: true
              # Doesn't intersect board
            | bcol == t-N then: false
            | otherwise: true
          end
        end
      end
  end
end

# new-game-state

fun new-board() -> Board:
  doc: ```Produces a new board.```
  lists.repeat(BOARD-DIMS.h + 2, 
      lists.repeat(BOARD-DIMS.w, t-N))
end

fun new-pieces-stream() -> Stream<Tetrimino>:
  doc: ```Produces a new piece stream.```

  fun new-piece() -> Tetrimino:
    doc: ```Produces a new piece.```
    TETRIMINO-PIECES.get(num-random(7))
  end

  lz-link(new-piece(), {(): new-pieces-stream()})
end

# tock

fun clear-lines(state :: State) -> State:
  doc: ```Produces a duplicate state with any
       full lines cleared away.```
  cases (State) state:
    | game-over => state
    | game(board, pieces, piece, score) =>
      # Find which rows to remove
      to-clear = board.filter(lam(row): not(row.any({(t): is-t-N(t)})) end)
      
      # Remove them and add empty rows to top
      nb = lists.repeat(to-clear.length(), lists.repeat(BOARD-DIMS.w, t-N))
        + board.filter({(row): not(to-clear.member(row))})
      
      game(nb, pieces, piece, score + to-clear.length())
  end
end

fun add-to-board(state :: State, 
    check-lose :: Boolean, clear :: Boolean) -> State:
  doc: ```Adds current piece to board;
       check-lose - true if should return game-over on loss;
       .            false for graphics mostly.
       clear - true if lines should be cleared if full;
       .       false for graphics mostly.```

  cases (State) state:
    | game-over(_, _, _) => state
    | game(board, pieces, piece, score) =>
      # Iterates through piece, updating board if
      # the tile is not empty.
      nb =
        for lists.fold_n(rown from piece.y, 
            nb2 from board, 
            row from piece.piece):
          
          for lists.fold_n(coln from piece.x, 
              nb3 from nb2, 
              col from row):
            if is-t-N(col):
              nb3
            else:
              nb3.set(rown,
                nb3.get(rown)
                  .set(coln, col))
            end
          end
        end
      
      # Clears full lines
      ns = clear-lines(game(nb, pieces.rest(), pieces.first, score))
      
      # Return based on check-lose and clear
      ask:
        | illegal-state(ns) and check-lose then: 
          game-over(ns.board, ns.pieces, ns.score)
        | clear then: ns
        | otherwise: game(nb, pieces.rest(), pieces.first, score)
      end
  end
end

fun move-down(state :: State) -> State:
  doc: ```Moves the current piece down.```
  cases (State) state:
    | game-over(_, _, _) => state
    | game(board, pieces, piece, score) =>
      game(board, pieces, 
        tetrimino(piece.piece, piece.x, piece.y + 1, piece.shape, piece.rot),
        score)
  end
end

# key-press

fun shift-piece(state :: State, dir :: Number) -> State:
  doc: ```Shifts the piece; -1 is left, 1 is right.```
  cases (State) state:
    | game-over(_, _, _) => state
    | game(board, pieces, piece, score) =>
      ns = game(board, pieces, 
        tetrimino(piece.piece, piece.x + dir, piece.y, piece.shape, piece.rot), 
        score)
      # Only shift if legal to shift
      if illegal-state(ns):
        state
      else:
        ns
      end
  end
end

# Tetris kicks for rotating when rotation causes illegal states
# Check in order from left to right; first legal position goes.
# If no legal positions out of list, turn fails.
# Each position is a change from current position; +x is right, +y is down.
CW-KICKS-NOTI = [list:
  [list: 
    {x: 0, y: 0}, {x: -1, y: 0}, {x: -1, y: -1}, {x: 0, y: 2},  {x: -1, y: 2}],
  [list: 
    {x: 0, y: 0}, {x: 1, y: 0},  {x: 1, y: 1},   {x: 0, y: -2}, {x: 1, y: -2}],
  [list: 
    {x: 0, y: 0}, {x: 1, y: 0},  {x: 1, y: -1},  {x: 0, y: 2},  {x: 1, y: 2}],
  [list: 
    {x: 0, y: 0}, {x: -1, y: 0}, {x: -1, y: 1},  {x: 0, y: -2}, {x: -1, y: -2}]]

CW-KICKS-I = [list:
  [list: 
    {x: 0, y: 0}, {x: -2, y: 0}, {x: 1, y: 0}, {x: -2, y: 1}, {x: 1, y: -2}],
  [list: 
    {x: 0, y: 0}, {x: -1, y: 0}, {x: 2, y: 0}, {x: -2, y: -1}, {x: 1, y: 2}],
  [list: 
    {x: 0, y: 0}, {x: 2, y: 0}, {x: -1, y: 0}, {x: 2, y: -1}, {x: -1, y: 2}],
  [list: 
    {x: 0, y: 0}, {x: 1, y: 0}, {x: -2, y: 0}, {x: 1, y: 2}, {x: -2, y: -1}]]


fun rotate-cw(p :: Board) -> Board:
  doc: ```Produces a cw-rotated piece.```
  ask:
      # O-pieces don't rotate
    | p.length() == 2 then: p
      # I-pieces are 4x4
    | p.length() == 4 then:
      fold4(lam(acc, e1, e2, e3, e4): 
          link([list: e4, e3, e2, e1], acc) 
        end, empty, 
        p.get(0), p.get(1), p.get(2), p.get(3)).reverse()
      # All other pieces are 3x3
    | otherwise:
      fold3(lam(acc, e1, e2, e3): 
          link([list: e3, e2, e1], acc) 
        end, empty, 
        p.get(0), p.get(1), p.get(2)).reverse()
  end
end

fun rotate-ccw(piece :: Board) -> Board:
  doc: ```Produces a ccw-rotated piece.```
  piece
    ^ rotate-cw
    ^ rotate-cw
    ^ rotate-cw
end

fun rotate-piece(state :: State, dir :: Number) -> State:
  doc: ```Rotates the piece; +1 is cw, -1 is ccw.```
  cases (State) state:
    | game-over(_, _, _) => state
    | game(board, pieces, piece, score) =>
      # Produce rotated copy of piece
      new-p = ask:
        | dir == 1 then: rotate-cw(piece.piece)
        | dir == -1 then: rotate-ccw(piece.piece)
      end
      
      # Generate proper kick sequence
      kicks = 
        map(lam(k): {x: k.x * dir, y: k.y * dir} end,
          cases (Tile) piece.shape:
            | t-O => [list: {x: 0, y: 0}]
            | t-I => CW-KICKS-I.get(piece.rot)
            | else => CW-KICKS-NOTI.get(piece.rot)
          end)
      
      # Apply from last to first; whichever is the last to work goes.
      new-p2 = kicks.foldr(lam(kick, curr):
          new-p3 = tetrimino(new-p, piece.x + kick.x, piece.y + kick.y, 
            piece.shape, num-remainder(piece.rot + dir + 4, 4))
          if illegal-state(game(board, pieces, new-p3, score)):
            curr
          else:
            new-p3
          end
        end, piece)
      
      game(board, pieces, new-p2, score)
  end
end

fun drop-piece(state :: State, clear :: Boolean) -> State:
  doc: ```Drops the piece to the bottom.```
  if illegal-state(move-down(state)):
    # If it is at teh bottom, add to board, check for loss, and clear lines.
    add-to-board(state, true, clear)
  else:
    # If it isn't at the bottom, recurse.
    drop-piece(move-down(state), clear)
  end
end

# draw-board

fun get-color(tile :: Tile) -> Color:
  doc: ```Produces the color of a tile.```
  cases (Tile) tile:
    | t-N => TILE-COLORS.N
    | t-H => TILE-COLORS.H
    | t-O => TILE-COLORS.O
    | t-I => TILE-COLORS.I
    | t-T => TILE-COLORS.T
    | t-L => TILE-COLORS.L
    | t-J => TILE-COLORS.J
    | t-S => TILE-COLORS.S
    | t-Z => TILE-COLORS.Z
  end
end

fun make-background() -> Image:
  doc: ```Produces the background rectangles of the game.```
  beside(
    # Board background
    rectangle(
      BOARD-DIMS.w * TILE-WIDTH, 
      BOARD-DIMS.h * TILE-WIDTH, 
      "solid",
      BG-COLOR),
    # Side-bar background
    rectangle(
      4 * TILE-WIDTH,
      BOARD-DIMS.h * TILE-WIDTH,
      "solid",
      SB-COLOR))
end

fun add-ghost(disp :: Image, state :: State) -> Image:
  doc: ```Adds the ghost of the tile to the board;
       Pretty hacky at the moment.```
  cases (State) state:
    | game-over(_, _, _) => disp
    | game(board, pieces, piece, score) =>
      # Replace the current piece with a duplicate, but with shadow colors.
      shad = piece.piece.map(_.map({(v): if v == t-N: t-N else: t-H end}))
      shad-piece = tetrimino(shad, piece.x, piece.y, t-N, piece.rot)
      shad-state = game(board, pieces, shad-piece, score)
      
      # Drop shadow piece to bottom of board, but don't clear lines!
      cases (State) drop-piece(shad-state, false):
        | game-over(_, _, _) => disp
        | game(board2, _, _, _) =>
          add-board(disp, board2)
      end
  end
end

fun add-board(disp :: Image, board :: Board) -> Image:
  doc: ```Adds the board to the background.```
  for lists.fold_n(rown from 0, disp2 from disp, row from board):
    for lists.fold_n(coln from 0, disp3 from disp2, col from row):
      underlay-xy(disp3,
        coln * TILE-WIDTH,
        rown * TILE-WIDTH,
        rectangle(TILE-WIDTH, TILE-WIDTH, "solid", get-color(col)))
    end
  end
end

fun add-next-piece(disp :: Image, piece :: Tetrimino) -> Image:
  doc: ```Adds the next piece in queue to the side-bar.```
  for lists.fold_n(rown from 2, disp2 from disp, row from piece.piece):
    for lists.fold_n(coln from BOARD-DIMS.w, disp3 from disp2, col from row):
      underlay-xy(disp3,
        (coln + ((4 - piece.piece.length()) * 0.5)) * TILE-WIDTH,
        (rown + 1.5) * TILE-WIDTH,
        rectangle(TILE-WIDTH, TILE-WIDTH, "solid", get-color(col)))
    end
  end
end

fun add-instructions(disp :: Image) -> Image:
  doc: ```Adds instructions to side-bar.```
  H = BOARD-DIMS.h - 1
  
  underlay-xy(disp,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    2.5 * TILE-WIDTH,
    text("Next Piece:", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    (H - 5) * TILE-WIDTH,
    text("← Left", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    (H - 4) * TILE-WIDTH,
    text("→ Right", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    (H - 3) * TILE-WIDTH,
    text("↑ Turn CW", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    (H - 2) * TILE-WIDTH,
    text("SHFT Turn CCW", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    (H - 1) * TILE-WIDTH,
    text("↓ Soft Drop", 
      20, black))
    ^ 
  underlay-xy(_,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    H * TILE-WIDTH,
    text("SPCE Hard Drop", 
      20, black))
end

fun add-score(disp :: Image, score :: Number) -> Image:
  doc: ```Adds score to side-bar.```
  underlay-xy(disp,
    (BOARD-DIMS.w + 0.3) * TILE-WIDTH,
    6.5 * TILE-WIDTH,
    text("Lines Cleared: " + num-to-string(score), 
      20, black))
end

fun add-game-over(disp :: Image) -> Image:
  doc: ```Adds "Game Over" to screen.```
  underlay-xy(disp,
    (BOARD-DIMS.w * 0.2) * TILE-WIDTH,
    (BOARD-DIMS.h * 0.5) * TILE-WIDTH,
    text-font("GAME OVER",
      40, red,
      "", "script",
      "normal", "bold",
      false))
end

fun crop-disp(disp :: Image) -> Image:
  doc: ```Crops away extra margin above board where pieces spawn.```
  crop(0, TILE-WIDTH * 2, 
    (BOARD-DIMS.w + 4) * TILE-WIDTH, (BOARD-DIMS.h - 2) * TILE-WIDTH, 
    disp)
end







#============= METHODS =============#

fun new-game-state() -> State:
  doc: ```Produces a new game state.```
  board = new-board()
  pieces = new-pieces-stream()
  piece = pieces.first
  game(board, pieces.rest(), piece, 0)
end

fun tock(state :: State) -> State:
  doc: ```Moves piece down on tick.```
  if illegal-state(move-down(state)):
    add-to-board(state, true, true)
  else:
    move-down(state)
  end
end

fun key-press(state :: State, key :: String) -> State:
  doc: ```Handles key presses.```
  ask:
    | key == "left" then: shift-piece(state, -1)
    | key == "right" then: shift-piece(state, 1)
    | key == "down" then: tock(state)
    | key == " " then: drop-piece(state, true)
    | key == "up" then: rotate-piece(state, 1)
    | key == "shift" then: rotate-piece(state, -1)
    | key == "escape" then: game-over
    | otherwise: state
  end
end

fun draw-board(state :: State) -> Image:
  doc: ```Draws current board state.```
  cases (State) add-to-board(state, false, false):
    | game-over(board, pieces, score) => 
      make-background()
        ^ add-board(_, board)
        ^ add-next-piece(_, pieces.first)
        ^ add-instructions(_)
        ^ add-score(_, score)
        ^ add-game-over(_)
        ^ crop-disp
    | game(board, _, next-piece, score) =>
      make-background()
        ^ add-ghost(_, state)
        ^ add-board(_, board)
        ^ add-next-piece(_, next-piece)
        ^ add-instructions(_)
        ^ add-score(_, score)
        ^ crop-disp
  end
end


#============= GAME INITIALIZATION =============#

r = reactor:
  init: new-game-state(),
    
  on-tick: tock,
  seconds-per-tick: 0.5,
  
  on-key: key-press,
  
  to-draw: draw-board,
  
  stop-when: is-game-over,
  close-when-stop: false,
  
  title: "Tetris"
end

interact(r)