from common import traverse, get_input


def is_end_node(node: str) -> bool:
    return node[-1] == 'Z'


graph, directions = get_input()
start_nodes = [node for node in graph.keys() if node[-1] == 'A']


print(traverse(graph, directions, start_nodes, is_end_node))
