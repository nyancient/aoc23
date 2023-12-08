from common import traverse, get_input


graph, directions = get_input()
print(traverse(graph, directions, ["AAA"]))
