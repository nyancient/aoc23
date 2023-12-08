import math
import re
import sys
import itertools
from typing import Callable


def parse_node(n: str) -> str:
    return n.split('=')[0].strip()


def parse_edges(n: str) -> tuple[str, str]:
    matches = re.match("[A-Z0-9]+=\\(([A-Z0-9]+),([A-Z09]+)\\)", n.replace(" ", ""))
    assert matches
    return (matches.group(1), matches.group(2))


def traverse(
    graph: dict[str, tuple[str, str]],
    directions: list[int],
    from_nodes: list[str] = ["AAA"],
    is_end_node: Callable[[str], bool] = lambda x: x == "ZZZ",
) -> int:
    distances_to_end = list[int]()
    for start_node in from_nodes:
        node = start_node
        steps = 0
        for direction in itertools.cycle(directions):
            if is_end_node(node):
                print(f"end node is {node}")
                distances_to_end.append(steps)
                break
            node = graph[node][direction]
            steps += 1
    
    return math.lcm(*distances_to_end)


def get_input() -> tuple[dict[str, tuple[str, str]], list[int]]:
    ds, *g = list(filter(lambda x: x.strip(), sys.stdin.readlines()))
    directions = [0 if d == "L" else 1 for d in ds.strip()]
    graph = {parse_node(n) : parse_edges(n) for n in g}
    return (graph, directions)
