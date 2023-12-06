import itertools
from common import solve


print(
    solve(
        times=itertools.islice(filter(lambda x: x, input().split(" ")), 1, None),
        records=itertools.islice(filter(lambda x: x, input().split(" ")), 1, None),
    )
)
