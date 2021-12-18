import functools
import re

SAMPLE_INPUT = [
    "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    "[[[[5,4],[7,7]],8],[[8,3],8]]",
    "[[9,3],[[9,9],[6,[4,9]]]]",
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]",
]

LAST = re.compile(r"(\d+)[^\d]*\Z")
FIRST = re.compile(r"\A[^\d]*(\d+)")
EXPLODE = re.compile(r"\[(\d+),(\d+)\]")
SPLIT = re.compile(r"\d{2,}")


def reduce(string):
    while True:
        for match in re.finditer(EXPLODE, string):
            before = string[: match.start()]
            depth = sum((c == "[") - (c == "]") for c in before)
            if depth > 3:
                after = string[match.end() :]
                if submatch := re.search(LAST, before):
                    before = f"{before[: submatch.start(1)]}{int(submatch.group(1)) + int(match.group(1))}{before[submatch.end(1) :]}"
                if submatch := re.search(FIRST, after):
                    after = f"{after[: submatch.start(1)]}{int(submatch.group(1)) + int(match.group(2))}{after[submatch.end(1) :]}"
                string = f"{before}0{after}"
                break
        else:
            if not (match := re.search(SPLIT, string)):
                break
            value = int(match.group(0))
            string = f"{string[: match.start()]}[{value // 2},{(value + 1) // 2}]{string[match.end() :]}"
    return string


def magnitude(string):
    def magnitude(i):
        assert string[i] == "["
        if string[i + 1].isdigit():
            lhs = int(string[i + 1])
            i += 2
        else:
            i, lhs = magnitude(i + 1)
        assert string[i] == ","
        if string[i + 1].isdigit():
            rhs = int(string[i + 1])
            i += 2
        else:
            i, rhs = magnitude(i + 1)
        assert string[i] == "]"
        return i + 1, 3 * lhs + 2 * rhs

    i, magnitude = magnitude(0)
    assert i == len(string)
    return magnitude


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    4140
    """
    return magnitude(
        functools.reduce(
            lambda x, y: reduce(f"[{x},{y}]"), (line.strip() for line in lines)
        )
    )


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    3993
    """
    return max(
        magnitude(reduce(f"[{x.strip()},{y.strip()}]"))
        for i, x in enumerate(lines)
        for j, y in enumerate(lines)
        if i != j
    )


parts = (part1, part2)
