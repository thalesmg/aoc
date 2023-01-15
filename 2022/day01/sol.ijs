NB. Rosetta Stone: https://github.com/Jlobblet/advent-of-code-2022/blob/main/src/day01/sol.ijs

raw_input =: 1!:1< 'input'
parsed =: (+/ @: > @: (".&.>) @: (LF cut ])) &.> LF2 splitnostring raw_input

part1 =: >. / > parsed
part2 =: +/ 3 {. \:~ > parsed

echo part1
echo part2
