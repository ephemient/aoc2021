def part1(lines):
    """
    >>> part1(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])
    150
    """
    x, y = 0, 0
    for line in lines:
        match line.partition(" "):
            case ("forward", _, d):
                x += int(d)
            case ("down", _, d):
                y += int(d)
            case ("up", _, d):
                y -= int(d)
    return x * y


def part2(lines):
    """
    >>> part2(["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])
    900
    """
    x, y, depth = 0, 0, 0
    for line in lines:
        match line.partition(" "):
            case ("forward", _, d):
                d = int(d)
                x += d
                y += d * depth
            case ("down", _, d):
                depth += int(d)
            case ("up", _, d):
                depth -= int(d)
    return x * y


parts = (part1, part2)
