pairs = {"(": ")", "[": "]", "{": "}", "<": ">"}
part1_points = {")": 3, "]": 57, "}": 1197, ">": 25137}
part2_points = {")": 1, "]": 2, "}": 3, ">": 4}


def part1(lines):
    """
    >>> part1(["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"])
    26397
    """
    total = 0
    for line in lines:
        expected = []
        for char in line.strip():
            if char in pairs:
                expected.append(pairs[char])
            elif not expected or char != expected.pop():
                total += part1_points[char]
                break
    return total


def part2(lines):
    """
    >>> part2(["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"])
    288957
    """
    scores = []
    for line in lines:
        expected = []
        for char in line.strip():
            if char in pairs:
                expected.append(pairs[char])
            elif not expected or char != expected.pop():
                break
        else:
            score = 0
            for char in expected[::-1]:
                score = 5 * score + part2_points[char]
            scores.append(score)
    scores.sort()
    return scores[len(scores) // 2]


parts = (part1, part2)
