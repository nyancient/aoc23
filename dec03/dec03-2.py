from collections import defaultdict
import re
import sys
from typing import Generator

def build_adjacency_table(schematic: list[str]) -> dict[tuple[int, int], list[int]]:
    number_regex = re.compile("([0-9]+)")
    adjacency_table = defaultdict[tuple[int, int], list[int]](list)
    for row, s in enumerate(schematic):
        y0 = max(0, row - 1)
        y1 = min(len(schematic) - 1, row + 1)
        for m in number_regex.finditer(s):
            x0 = max(0, m.span()[0] - 1)
            x1 = min(len(s) - 1, m.span()[1])
            adjacent_coords = (
                (x, y)
                for y in range(y0, y1 + 1)
                for x in range(x0, x1 + 1)
                if schematic[y][x] == '*'
            )
            for x, y in adjacent_coords:
                adjacency_table[x, y].append(int(m.group()))
    return adjacency_table

def get_gear_ratios(adj_table: dict[tuple[int, int], list[int]], schematic: list[str]) -> Generator[int, None, None]:
    for y, s in enumerate(schematic):
        for x, c in enumerate(s):
            if numbers := adj_table[x, y]:
                if c == '*' and len(numbers) == 2:
                    yield numbers[0] * numbers[1]

schematic = [ln.strip() for ln in sys.stdin.readlines()]
adjacency_table = build_adjacency_table(schematic)
gear_ratio_sum = sum(get_gear_ratios(adjacency_table, schematic))
print(gear_ratio_sum)
