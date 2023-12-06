import math
from typing import Iterable


def get_roots(a: float | int, b: float | int, c: float | int) -> tuple[float, float]:
    """Returns the roots of the quadratic equation 0 = ax^2 + bx + c."""
    inner = math.sqrt(b * b - 4 * a * c)
    x0 = (-b + inner) / 2 * a
    x1 = (-b - inner) / 2 * a
    return (x0, x1)


def get_ways_to_beat_record(time: int, distance: int) -> int:
    x0, x1 = get_roots(-1, time, -distance)
    return math.ceil(x1) - math.floor(x0 + 1)


def solve(times: Iterable[str], records: Iterable[str]) -> int:
    return math.prod(get_ways_to_beat_record(int(t), int(r)) for t, r in zip(times, records))
