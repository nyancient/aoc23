from enum import Enum
import sys
from typing import Generator
import click


class SpaceThing(Enum):
    EMPTY = 0
    GALAXY = 1


type Loc = tuple[int, int]


class Space:
    space: list[list[SpaceThing]]
    expansion_factor: int
    empty_rows: set[int]
    empty_cols: set[int]

    def __init__(self, lns: list[str], expansion_factor: int) -> None:
        self.space = [
            [SpaceThing.EMPTY if c == "." else SpaceThing.GALAXY for c in ln.strip()]
            for ln in lns
            if not ln.isspace()
        ]
        self.expansion_factor = expansion_factor
        self.empty_rows = {
            i
            for i, ln in enumerate(self.space)
            if all(c == SpaceThing.EMPTY for c in ln)
        }
        self.empty_cols = {
            i
            for i, ln in enumerate(map(list, zip(*self.space)))
            if all(c == SpaceThing.EMPTY for c in ln)
        }

    def galaxies(self, skip: int = 0) -> Generator[Loc, None, None]:
        for y, ln in enumerate(self.space):
            for x, c in enumerate(ln):
                if c == SpaceThing.GALAXY:
                    if skip <= 0:
                        yield (x, y)
                    else:
                        skip -= 1

    def taxicab_distance(self, a: Loc, b: Loc) -> int:
        x0 = min(a[0], b[0])
        x1 = max(a[0], b[0])
        y0 = min(a[1], b[1])
        y1 = max(a[1], b[1])
        empty_rows_on_path = len([y for y in range(y0, y1) if y in self.empty_rows])
        empty_cols_on_path = len([x for x in range(x0, x1) if x in self.empty_cols])
        x_dist = x1 - x0 + (self.expansion_factor - 1) * empty_cols_on_path
        y_dist = y1 - y0 + (self.expansion_factor - 1) * empty_rows_on_path
        return y_dist + x_dist


@click.command()
@click.option("-e", "--expansion-factor", default=2)
def main(expansion_factor: int) -> None:
    space = Space(sys.stdin.readlines(), expansion_factor)

    shortest_distances: list[int] = []
    for i, from_galaxy in enumerate(space.galaxies()):
        for to_galaxy in space.galaxies(i + 1):
            dist = space.taxicab_distance(from_galaxy, to_galaxy)
            shortest_distances.append(dist)

    print(sum(shortest_distances))


if __name__ == "__main__":
    main()
