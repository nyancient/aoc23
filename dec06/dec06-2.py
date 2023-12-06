import itertools
from common import solve


print(
    solve(
        times=["".join(itertools.islice(input().split(" "), 1, None))],
        records=["".join(itertools.islice(input().split(" "), 1, None))],
    )
)
