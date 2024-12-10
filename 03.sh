cat inputs/3.in |
  sed -E "s/(mul\([[:digit:]]{1,3},[[:digit:]]{1,3}\))/\n\1\n/g" |
  grep "mul([[:digit:]]\+,[[:digit:]]\+)" |
  sed 's/mul(\([[:digit:]]\+\),\([[:digit:]]\+\))/\1 \2/g' |
  awk 'sum += $1 * $2; END { print sum }'


cat inputs/3.in |
  sed -E "s/(do\(\)|don't\(\)|mul\([[:digit:]]{1,3},[[:digit:]]{1,3}\))/\n\1\n/g" |
  grep "do()\|don't()\|mul([[:digit:]]\+,[[:digit:]]\+)" |
  sed "s/do()/@/g" |
  tr -d '\n' |
  sed "s/don't()[^@]\+@//g" |
  sed "s/don't()[^@]\+$//" |
  sed "s/\(mul([[:digit:]]\+,[[:digit:]]\+)\)/\n\1\n/g" |
  grep "mul" |
  sed "s/mul(\([[:digit:]]\+\),\([[:digit:]]\+\))/\1 \2/g" |
  awk 'sum += $1 * $2; END { print sum }'

