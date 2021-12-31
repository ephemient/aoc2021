SAMPLE_INPUT = [
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "inp w",
    "mul z 31",
    "add z w",
    "mod z 16777216",
]


class ALU:
    __slots__ = ("w", "x", "y", "z")

    def __init__(self, w, x, y, z):
        self.w, self.x, self.y, self.z = w, x, y, z

    def copy(self):
        return ALU(self.w, self.x, self.y, self.z)


def solve(lines, nums, prefix=0, alu=None):
    if not alu:
        alu = ALU(0, 0, 0, 0)
    for i, line in enumerate(lines):
        match line.split():
            case "inp", lhs:
                for x in nums:
                    setattr(alu, lhs, x)
                    if skip_range(lines[i + 1 :], alu.w, alu.x, alu.y, alu.z):
                        continue
                    r = solve(
                        lines[i + 1 :], nums, prefix=10 * prefix + x, alu=alu.copy()
                    )
                    if r is not None:
                        return r
                return None
            case "add", lhs, rhs:
                setattr(
                    alu,
                    lhs,
                    getattr(alu, lhs)
                    + (int(rhs) if rhs.lstrip("-").isnumeric() else getattr(alu, rhs)),
                )
            case "mul", lhs, rhs:
                setattr(
                    alu,
                    lhs,
                    getattr(alu, lhs)
                    * (int(rhs) if rhs.lstrip("-").isnumeric() else getattr(alu, rhs)),
                )
            case "div", lhs, rhs:
                setattr(
                    alu,
                    lhs,
                    getattr(alu, lhs)
                    // (int(rhs) if rhs.lstrip("-").isnumeric() else getattr(alu, rhs)),
                )
            case "mod", lhs, rhs:
                setattr(
                    alu,
                    lhs,
                    getattr(alu, lhs)
                    % (int(rhs) if rhs.lstrip("-").isnumeric() else getattr(alu, rhs)),
                )
            case "eql", lhs, rhs:
                setattr(
                    alu,
                    lhs,
                    int(
                        getattr(alu, lhs)
                        == (
                            int(rhs)
                            if rhs.lstrip("-").isnumeric()
                            else getattr(alu, rhs)
                        )
                    ),
                )
    return None if alu.z else prefix


def skip_range(lines, w, x, y, z):
    alu = ALU((w, w), (x, x), (y, y), (z, z))
    for line in lines:
        match line.split():
            case "inp", lhs:
                setattr(alu, lhs, (1, 9))
            case "add", lhs, rhs:
                a, b = getattr(alu, lhs)
                c, d = (
                    (int(rhs), int(rhs))
                    if rhs.lstrip("-").isnumeric()
                    else getattr(alu, rhs)
                )
                setattr(alu, lhs, (a + c, b + d))
            case "mul", lhs, rhs:
                a, b = getattr(alu, lhs)
                c, d = (
                    (int(rhs), int(rhs))
                    if rhs.lstrip("-").isnumeric()
                    else getattr(alu, rhs)
                )
                if a >= 0 and c >= 0:
                    r = a * c, b * d
                elif b <= 0 and d <= 0:
                    r = b * d, a * c
                elif a >= 0 and d <= 0:
                    r = a * d, b * c
                elif b <= 0 and c >= 0:
                    r = b * c, a * d
                else:
                    xs = [0, a * c, a * d, b * c, b * d]
                    r = min(xs), max(xs)
                setattr(alu, lhs, r)
            case "div", lhs, rhs:
                a, b = getattr(alu, lhs)
                c, d = (
                    (int(rhs), int(rhs))
                    if rhs.lstrip("-").isnumeric()
                    else getattr(alu, rhs)
                )
                if c > 0:
                    r = a // d, b // c
                elif d < 0:
                    r = a // c, b // d
                else:
                    return None
                setattr(alu, lhs, r)
            case "mod", lhs, rhs:
                a, b = getattr(alu, lhs)
                c, d = (
                    (int(rhs), int(rhs))
                    if rhs.lstrip("-").isnumeric()
                    else getattr(alu, rhs)
                )
                if 0 < c == d:
                    if b - a + 1 < c and a % c <= b % c:
                        r = a % c, b % c
                    else:
                        r = 0, c - 1
                else:
                    return None
                setattr(alu, lhs, r)
            case "eql", lhs, rhs:
                a, b = getattr(alu, lhs)
                c, d = (
                    (int(rhs), int(rhs))
                    if rhs.lstrip("-").isnumeric()
                    else getattr(alu, rhs)
                )
                if a == b == c == d:
                    r = 1, 1
                elif a <= d and c <= b:
                    r = 0, 1
                else:
                    r = 0, 0
                setattr(alu, lhs, r)
    z0, z1 = alu.z
    return z0 > 0 or 0 > z1


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    99999993811817
    """
    return solve(lines, range(9, 0, -1))


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    11111128657365
    """
    return solve(lines, range(1, 10))


parts = (part1, part2)
