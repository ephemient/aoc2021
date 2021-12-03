from collections import Counter


def part1(lines):
    """
    >>> part1(["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"])
    198
    """
    gamma = "".join(Counter(column).most_common(1)[0][0] for column in zip(*lines))
    epsilon = "".join(chr(ord(c) ^ 1) for c in gamma)
    return int(gamma, 2) * int(epsilon, 2)


def part2(lines):
    """
    >>> part2(["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"])
    230
    """
    o2 = lines
    co2 = lines
    i = 0
    while len(o2) > 1 or len(co2) > 1:
        common = max(
            Counter(s[i] for s in o2).items(), key=lambda entry: (entry[1], entry[0])
        )[0]
        uncommon = min(
            Counter(s[i] for s in co2).items(), key=lambda entry: (entry[1], entry[0])
        )[0]
        o2 = [s for s in o2 if s[i] == common]
        co2 = [s for s in co2 if s[i] == uncommon]
        i += 1
    return int(o2[0], 2) * int(co2[0], 2)


parts = (part1, part2)
