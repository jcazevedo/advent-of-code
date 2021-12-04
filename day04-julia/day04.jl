input_string = open(f->read(f, String), "inputs/04.input")
input_lines = split(input_string, '\n', keepempty = false)
numbers = map(x -> parse(Int64, x), split(input_lines[1], ','))

function build_boards(n)
  ans = Vector{Vector{Vector{Int64}}}()
  curr = Vector{Vector{Int64}}()
  curr_idx = 0
  for line in 2:length(input_lines)
    row = map(x -> parse(Int64, x), split(input_lines[line], r"\s+", keepempty = false))
    push!(curr, row)
    curr_idx = curr_idx + 1
    if curr_idx == n
      push!(ans, curr)
      curr = Vector{Vector{Int64}}()
      curr_idx = 0
    end
  end
  ans
end

boards = build_boards(5)

function draw(board, number)
  n = length(board)
  for i in 1:n
    for j in 1:n
      if board[i][j] == number
        board[i][j] = 0
      end
    end
  end
end

function wins(board)
  n = length(board)
  has_row = false
  for i in 1:n
    tmp = true
    for j in 1:n
      if board[i][j] != 0
        tmp = false
      end
    end
    if tmp
      has_row = true
      break
    end
  end
  if has_row
    return has_row
  end
  has_col = false
  for j in 1:n
    tmp = true
    for i in 1:n
      if board[i][j] != 0
        tmp = false
      end
    end
    if tmp
      has_col = true
      break
    end
  end
  return has_col
end

function sum_unmarked(board)
  ans = 0
  n = length(board)
  for row in board
    for num in row
      ans = ans + num
    end
  end
  ans
end

function go()
  draw_idx = 1
  total_won = 0
  n_boards = length(boards)
  won = zeros(n_boards)
  won_first = 0
  won_last = 0
  while true
    draw_number = numbers[draw_idx]
    for i in 1:n_boards
      if won[i] == 1
        continue
      end
      draw(boards[i], draw_number)
      if wins(boards[i])
        won[i] = 1
        total_won = total_won + 1
        if total_won == 1
          won_first = draw_number * sum_unmarked(boards[i])
        elseif total_won == n_boards
          won_last = draw_number * sum_unmarked(boards[i])
          return (won_first, won_last)
        end
      end
    end
    draw_idx = draw_idx + 1
  end
end

(part1, part2) = go()

println("Part 1: " * string(part1))
println("Part 2: " * string(part2))
