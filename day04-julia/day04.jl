input_string = open(f->read(f, String), "inputs/04.input")
input_lines = split(input_string, '\n', keepempty=false)
numbers = map(x -> parse(Int64, x), split(input_lines[1], ','))

function build_boards(n)
  ans = Vector{Matrix}()
  curr = Vector{Vector{Int64}}()
  curr_idx = 0
  for line in 2:length(input_lines)
    row = map(x -> parse(Int64, x), split(input_lines[line], r"\s+", keepempty=false))
    push!(curr, row)
    curr_idx = curr_idx + 1
    if curr_idx == n
      push!(ans, hcat(curr...))
      curr = Vector{Vector{Int64}}()
      curr_idx = 0
    end
  end
  ans
end

boards = build_boards(5)

function draw(board, number)
  map(row -> map(num -> if num == number 0 else num end, row), board)
end

function wins(board)
  row_sums = sum(board, dims=1)
  col_sums = sum(board, dims=2)
  return length(filter(x -> x == 0, col_sums)) > 0 || length(filter(x -> x == 0, row_sums)) > 0
end

function sum_unmarked(board)
  sum(map(row -> sum(row), board))
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
      boards[i] = draw(boards[i], draw_number)
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
