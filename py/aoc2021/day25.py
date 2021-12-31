def part1(lines):
    """
    >>> part1(["v...>>.vv>", ".vv>>.vv..", ">>.>v>...v", ">>v>>.>.v.", "v>v.vv.v..", ">.>>..v...", ".vv..>.>v.", "v.v..>>v.v", "....v..v.>"])
    58
    """
    grid = [list(line.strip()) for line in lines]
    n = 1
    while True:
        a, b = False, False
        for row in grid:
            indices = [
                i
                for i in range(len(row))
                if row[i] == ">" and row[(i + 1) % len(row)] == "."
            ]
            if indices:
                a = True
            for i in indices:
                row[i] = "."
                row[(i + 1) % len(row)] = ">"
        for col in range(len(grid[0])):
            indices = [
                i
                for i in range(len(grid))
                if grid[i][col] == "v" and grid[(i + 1) % len(grid)][col] == "."
            ]
            if indices:
                b = True
            for i in indices:
                grid[i][col] = "."
                grid[(i + 1) % len(grid)][col] = "v"
        if not (a or b):
            return n
        n += 1


parts = (part1,)
