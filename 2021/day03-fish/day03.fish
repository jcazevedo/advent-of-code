set input

while read -l line
  set -a input $line
end < inputs/03.input

function part1
  set cnts_0
  set cnts_1
  for num in $argv
    set idx 1
    for i in (string split '' $num)
      if test "$i" = "1"
        set cnts_1[$idx] (math "$cnts_1[$idx] + 1")
      else
        set cnts_0[$idx] (math "$cnts_0[$idx] + 1")
      end
      set idx (math "$idx + 1")
    end
  end
  set gamma_rate 0
  set epsilon_rate 0
  for idx in (seq (count $cnts_0))
    set gamma_rate (math "$gamma_rate * 2")
    set epsilon_rate (math "$epsilon_rate * 2")
    if test $cnts_1[$idx] -gt $cnts_0[$idx]
      set gamma_rate (math "$gamma_rate + 1")
    else
      set epsilon_rate (math "$epsilon_rate + 1")
    end
  end
  echo (math "$gamma_rate * $epsilon_rate")
end

function binary_to_decimal
  set ans 0
  for num in (string split '' $argv[1])
    set ans (math "$ans * 2 + $num")
  end
  echo $ans
end

function part2
  set oxygen_generator $argv
  set idx 1
  while test (count $oxygen_generator) -gt 1
    set cnts_0 0
    set cnts_1 0
    for num in $oxygen_generator
      set num_arr (string split '' $num)
      if test "$num_arr[$idx]" = "1"
        set cnts_1 (math "$cnts_1 + 1")
      else
        set cnts_0 (math "$cnts_0 + 1")
      end
    end
    set next_oxygen_generator
    set target 1
    if test $cnts_0 -gt $cnts_1
      set target 0
    end
    for num in $oxygen_generator
      set num_arr (string split '' $num)
      if test "$num_arr[$idx]" = "$target"
        set -a next_oxygen_generator $num
      end
    end
    set oxygen_generator $next_oxygen_generator
    set idx (math "$idx + 1")
  end
  set co2_scrubber $argv
  set idx 1
  while test (count $co2_scrubber) -gt 1
    set cnts_0 0
    set cnts_1 0
    for num in $co2_scrubber
      set num_arr (string split '' $num)
      if test "$num_arr[$idx]" = "1"
        set cnts_1 (math "$cnts_1 + 1")
      else
        set cnts_0 (math "$cnts_0 + 1")
      end
    end
    set next_co2_scrubber
    set target 0
    if test $cnts_1 -lt $cnts_0
      set target 1
    end
    for num in $co2_scrubber
      set num_arr (string split '' $num)
      if test "$num_arr[$idx]" = "$target"
        set -a next_co2_scrubber $num
      end
    end
    set co2_scrubber $next_co2_scrubber
    set idx (math "$idx + 1")
  end
  set oxygen_generator (binary_to_decimal $oxygen_generator)
  set co2_scrubber (binary_to_decimal $co2_scrubber)
  echo (math "$oxygen_generator * $co2_scrubber")
end

echo "Part 1:" (part1 $input)
echo "Part 2:" (part2 $input)
