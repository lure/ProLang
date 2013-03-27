# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris

  def key_bindings
    super
    @root.bind('u', proc { @board.rotate_180degree })
    @root.bind('c', proc { @board.enable_cheat })
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end


end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = [
      [[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
      rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
      [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
       [[0, 0], [0, -1], [0, 1], [0, 2]]],
      rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
      rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
      rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
      rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
      rotations([[0, 0], [1, 0], [0, -1]]), # |_
      [[[-1, 0], [-2, 0], [0, 0], [1, 0], [2, 0]], # 5-long (only needs two)
       [[0, -1], [0, -2], [0, 0], [0, 1], [0, 2]]],
      rotations([[0, 0], [-1, 0], [1, 0], [0, 1], [1, 1]])
  ]

  My_Cheat_Piece =[[[0, 0]]]

  # your enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    if !board.cheat
      MyPiece.new(All_My_Pieces.sample, board)
    else
      board.cheat = false
      MyPiece.new(My_Cheat_Piece, board)
    end
  end


end

class MyBoard < Board
  # your enhancements here
  attr_accessor :cheat

  def initialize (game)
    @cheat = false
    @grid = Array.new(num_rows) { Array.new(num_columns) }
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  # rotates the current piece counterclockwise
  def rotate_180degree
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each { |index|
      current = locations[index]
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def enable_cheat
    if (!@cheat) && (@score >= 100)
      @score -= 100
      @cheat = true
    end
  end
end
