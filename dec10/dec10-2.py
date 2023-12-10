from collections import deque
import sys


type Loc = tuple[int, int]


zooms = {
    "|": [" █ █ ", " █ █ ", " █ █ ", " █ █ ", " █ █ "],
    "-": ["     ", "█████", "     ", "█████", "     "],
    "7": ["     ", "████ ", "   █ ", "██ █ ", " █ █ "],
    "J": [" █ █ ", "██ █ ", "   █ ", "████ ", "     "],
    "F": ["     ", " ████", " █   ", " █ ██", " █ █ "],
    "L": [" █ █ ", " █ ██", " █   ", " ████", "     "],
    ".": ["     ", "     ", "     ", "     ", "     "],
}

class Graph:
    original: list[list[str]]
    zoomed: list[list[str]]
    start_location: Loc

    def __init__(self, lns: list[str]) -> None:
        self.original = [[c for c in ln] for ln in lns]
        self.zoomed = self._zoom()

    def _infer_start_node_type(self, x: int, y: int) -> str:
        if x > 0 and self.original[y][x - 1] in "-LF":
            if x < len(self.original[y]) - 1 and self.original[y][x + 1] in "-J7":
                return "-"
            elif y > 0 and self.original[y - 1][x] in "|7F":
                return "J"
            else:
                return "7"
        elif x < len(self.original[y]) - 1 and self.original[y][x + 1] in "-J7":
            if x > 0 and self.original[y][x - 1] in "-FL":
                return "-"
            elif y > 0 and self.original[y - 1][x] in "|7F":
                return "L"
            else:
                return "F"
        else:
            return "|"

    def _zoom(self) -> list[list[str]]:
        out: list[list[str]] = []
        for y, ln in enumerate(self.original):
            zoomed_ln: list[list[str]] = [[], [], [], [], []]
            for x, cell in enumerate(ln):
                def fmt(str: str) -> str:
                    return str
                if cell == "S":
                    self.start_location = (x*5 + 2, y*5 + 2)
                    cell = self._infer_start_node_type(x, y)
                    def fmt(str: str) -> str:
                        return str.replace("█", "░")
                for i, zln in enumerate(zooms[cell]):
                    zoomed_ln[i].extend(fmt(c) for c in zln)
            out.extend(zoomed_ln)
        return out

    def free_locations(self) -> set[Loc]:
        return {(x, y) for y, ln in enumerate(self.zoomed) for x, c in enumerate(ln) if c == " "}

    def dead_locations(self, loop_char: str) -> set[Loc]:
        return {
            (x, y)
            for y, ln in enumerate(self.zoomed)
            for x, c in enumerate(ln)
            if c in ("█", "░") and all(
                c != loop_char
                for ln in self.zoomed[max(y-1, 0):y+2]
                for c in ln[max(x-1, 0):x+2]
            )
        }

    def is_free(self, x: int, y: int) -> bool:
        if x < 0 or y < 0 or y >= len(self.zoomed) or x >= len(self.zoomed[y]):
            return False
        return self.zoomed[y][x] == " "

    def floodfill(self, x: int, y: int, c: str) -> set[Loc]:
        filled_locs: set[Loc] = set()
        queue: deque[Loc] = deque()
        queue.append((x, y))
        while queue:
            x, y = queue.popleft()
            if self.is_free(x, y):
                self.zoomed[y][x] = c
                filled_locs.add((x, y))
                queue.extend((a, b) for a in [x-1, x, x+1] for b in [y-1, y, y+1])
        return filled_locs
            
    def print(self) -> None:
        for ln in self.zoomed:
            print(''.join(ln))

    def floodfill_loop(self, fill_char: str) -> None:
        self.floodfill(*self.start_location, fill_char)

    def erase_dead_pipes(self, loop_char: str) -> None:
        for x, y in self.dead_locations(loop_char):
            self.zoomed[y][x] = " "
            self.original[y // 5][x // 5] = "."

    def is_free_in_original(self, x: int, y: int) -> bool:
        return self.original[y // 5][x // 5] == "."


lines = list(filter(lambda x: x, map(lambda x: x.strip(), sys.stdin.readlines())))
graph = Graph(lines)
graph.floodfill_loop("*")
graph.erase_dead_pipes("*")
graph.floodfill(0, 0, "x")
contained_locs = {(x // 5, y // 5) for x, y in graph.free_locations() if graph.is_free_in_original(x, y)}
print(len(contained_locs))