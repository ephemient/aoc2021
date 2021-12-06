import functools

step = (
    (0, 1, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 1, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 1, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 1, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 1, 0, 0),
    (1, 0, 0, 0, 0, 0, 0, 1, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 1),
    (1, 0, 0, 0, 0, 0, 0, 0, 0),
)


def mul(x, y):
    cols = min(len(row) for row in y)
    return tuple(
        tuple(sum(a * b[j] for a, b in zip(row, y)) for j in range(cols))
        for i, row in enumerate(x)
    )


def pow(base, e):
    if e <= 1:
        return base
    double = pow(mul(base, base), e // 2)
    return mul(double, base) if e % 2 else double


def precompute(times):
    """
    >>> precompute(80)
    (1421, 1401, 1191, 1154, 1034, 950, 905, 779, 768)
    >>> precompute(256)
    (6703087164, 6206821033, 5617089148, 5217223242, 4726100874, 4368232009, 3989468462, 3649885552, 3369186778)
    """
    matrix = pow(step, times)
    return tuple(map(sum, zip(*matrix)))


def solve_(constants, lines):
    """
    >>> part1(["3,4,3,1,2"])
    5934
    >>> part2(["3,4,3,1,2"])
    26984457539
    """
    return sum(constants[int(num)] for line in lines for num in line.strip().split(","))


part1 = functools.partial(solve_, precompute(80))
part2 = functools.partial(solve_, precompute(256))


parts = (part1, part2)
