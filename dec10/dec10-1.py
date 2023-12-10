import sys
from collections import defaultdict, deque
from typing import Optional


type Node = tuple[int, int]
type Edge = tuple[Node, Node]


connections = {
    "|": [((0, -1), "|7FS"), ((0, 1), "|LJS")],
    "-": [((-1, 0), "-LFS"), ((1, 0), "-J7S")],
    "L": [((0, -1), "|7FS"), ((1, 0), "-J7S")],
    "J": [((0, -1), "|7FS"), ((-1, 0), "-LFS")],
    "7": [((-1, 0), "-LFS"), ((0, 1), "|LJS")],
    "F": [((1, 0), "-J7S"), ((0, 1), "|LJS")],
    ".": [],
    "S": [((0, -1), "|7FS"), ((0, 1), "|LJS"), ((-1, 0), "-LFS"), ((1, 0), "-J7S")],
}


class Graph:
    nodes: set[Node]
    edges: dict[Node, set[Edge]]
    map: list[str]
    start_node: Node

    def __init__(self, map: list[str]) -> None:
        self.map = map
        self.start_node = (0, 0)
        self.nodes = {
            (x, y)
            for y, ln in enumerate(self.map)
            for x, cell in enumerate(ln)
            if cell != "."
        }
        self._fill_in_edges()

    def get_node_type(self, node: Node) -> Optional[str]:
        if node[1] < 0 or node[1] >= len(self.map):
            return None
        if node[0] < 0 or node[0] >= len(self.map[node[1]]):
            return None
        return self.map[node[1]][node[0]]

    def _fill_in_edges(self) -> None:
        edges: dict[Node, set[Edge]] = defaultdict(set)
        for y, ln in enumerate(self.map):
            for x, cell in enumerate(ln):
                if cell == "S":
                    self.start_node = (x, y)
                    cell = self._infer_start_node_type(x, y)
                for node_diff, ok_connections in connections[cell]:
                    target_node = (x + node_diff[0], y + node_diff[1])
                    target_node_type = self.get_node_type(target_node)
                    if target_node_type and target_node_type in ok_connections:
                        edges[x, y].add(((x, y), target_node))
        self.edges = edges

    def _infer_start_node_type(self, x: int, y: int) -> str:
        if self.map[y][x - 1] in "-LF":
            if self.map[y][x + 1] in "-J7":
                return "-"
            elif self.map[y - 1][x] in "|7F":
                return "J"
            else:
                return "7"
        elif self.map[y][x + 1] in "-J7":
            if self.map[y][x - 1] in "-J7":
                return "-"
            elif self.map[y - 1][x] in "|7F":
                return "L"
            else:
                return "F"
        else:
            return "|"


def get_start_node(lns: list[str]) -> Node:
    return next(
        (x, y)
        for y, ln in enumerate(lns)
        for x, cell in enumerate(ln)
        if cell == "S"
    )


def traverse_graph(graph: Graph) -> dict[Node, list[Edge]]:
    visited_nodes: dict[Node, list[Edge]] = dict()
    queue: deque[tuple[Node, list[Edge]]] = deque()
    queue.append((graph.start_node, []))
    while queue:
        node, path = queue.popleft()
        if node not in visited_nodes:
            visited_nodes[node] = path
            queue.extend((edge[1], [*path, edge]) for edge in graph.edges[node])
    return visited_nodes


def longest_path(graph: Graph) -> list[Edge]:
    return max(traverse_graph(graph).values(), key=len)


lines = list(filter(lambda x: x, map(lambda x: x.strip(), sys.stdin.readlines())))
graph = Graph(lines)
print(len(longest_path(graph)))
