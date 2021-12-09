from math import prod


def part1(lines):
    """
    >>> part1(["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"])
    15
    """
    return sum(
        int(c) + 1
        for i, line in enumerate(lines)
        for j, c in enumerate(line.strip())
        if "0" <= c <= "9"
        and all(
            c < lines[i2][j2]
            for (i2, j2) in ((i - 1, j), (i, j - 1), (i, j + 1), (i + 1, j))
            if 0 <= i2 < len(lines) and 0 <= j2 < len(lines[i2].strip())
        )
    )


def part2(lines):
    """
    >>> part2(["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"])
    1134
    """
    basins = []
    visited = [[False for _ in line.strip()] for line in lines]
    for i, line in enumerate(lines):
        for j, c in enumerate(line.strip()):
            if visited[i][j] or not ("0" <= c < "9"):
                continue
            basin = {(i, j)}
            stack = [(i, j)]
            while stack:
                i2, j2 = stack.pop()
                visited[i2][j2] = True
                for i3, j3 in ((i2 - 1, j2), (i2, j2 - 1), (i2, j2 + 1), (i2 + 1, j2)):
                    if (
                        0 <= i3 < len(lines)
                        and 0 <= j3 < len(lines[i3].strip())
                        and "0" <= lines[i3][j3] < "9"
                        and (i3, j3) not in basin
                    ):
                        basin.add((i3, j3))
                        stack.append((i3, j3))
            basins.append(len(basin))
    return prod(sorted(basins)[-3:])


parts = (part1, part2)
