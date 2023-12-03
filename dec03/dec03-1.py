import re
import sys
from typing import Generator

def get_part_numbers(schematic: list[str]) -> Generator[int, None, None]:
    number_regex = re.compile("([0-9]+)")
    for row, s in enumerate(schematic):
        y0 = max(0, row - 1)
        y1 = min(len(schematic) - 1, row + 1)
        for m in number_regex.finditer(s):
            x0 = max(0, m.span()[0] - 1)
            x1 = min(len(s) - 1, m.span()[1])
            adjacent_symbol = any(
                True
                for y in range(y0, y1 + 1)
                for x in range(x0, x1 + 1)
                if schematic[y][x] not in '0123456789.'
            )
            if adjacent_symbol:
                yield int(m.group())            

part_numbers = get_part_numbers([ln.strip() for ln in sys.stdin.readlines()])
print(sum(part_numbers))
