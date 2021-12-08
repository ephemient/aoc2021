from collections import defaultdict
from functools import reduce

SAMPLE_INPUT_1 = [
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf",
]

SAMPLE_INPUT_2 = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce",
]


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT_2)
    26
    """
    return sum(
        len(word) in (2, 4, 3, 7)
        for line in lines
        for word in line.split(" | ", maxsplit=2)[-1].strip().split()
    )


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT_1)
    5353
    >>> part2(SAMPLE_INPUT_2)
    61229
    """
    total = 0
    for line in lines:
        lhs, rhs = line.strip().split(" | ", maxsplit=2)
        signals = defaultdict(list)
        for signal in lhs.split():
            signals[len(signal)].append(set(signal))
        (one,) = signals[2]
        (seven,) = signals[3]
        (four,) = signals[4]
        (*three_five, two) = sorted(signals[5], key=(one | four).__rsub__)
        (three, five) = sorted(three_five, key=two.__rsub__)
        (*zero_nine, six) = sorted(signals[6], key=one.__sub__)
        (nine, zero) = sorted(zero_nine, key=three.__rsub__)
        (eight,) = signals[7]
        digits = (zero, one, two, three, four, five, six, seven, eight, nine)
        output = [digits.index(set(output)) for output in rhs.split()]
        total += reduce(lambda x, y: 10 * x + y, output)
    return total


parts = (part1, part2)
