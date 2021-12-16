import itertools
from collections import namedtuple
from math import prod

Literal = namedtuple("Literal", ("version", "value"))
Operator = namedtuple("Operator", ("version", "type", "children"))


def hex_to_bits(it):
    for c in it:
        n = int(c, 16)
        yield n & 8 != 0
        yield n & 4 != 0
        yield n & 2 != 0
        yield n & 1 != 0


def next_bits(it, n):
    value = 0
    for _ in range(n):
        value = 2 * value + next(it)
    return value


def parse_packet(it):
    version = next_bits(it, 3)
    type = next_bits(it, 3)
    if type == 4:
        value = 0
        while True:
            cont = next(it)
            value = 16 * value + next_bits(it, 4)
            if not cont:
                return Literal(version=version, value=value)
    if next(it):
        children = [parse_packet(it) for _ in range(next_bits(it, 11))]
    else:
        children = []
        it2 = itertools.islice(it, next_bits(it, 15))
        try:
            while True:
                children.append(parse_packet(it2))
        except StopIteration:
            pass
    return Operator(version=version, type=type, children=children)


def sum_versions(packet):
    match packet:
        case Literal(version=version):
            return version
        case Operator(version=version, children=children):
            return version + sum(map(sum_versions, children))


def eval_packet(packet):
    match packet:
        case Literal(value=value):
            return value
        case Operator(type=0, children=children):
            return sum(map(eval_packet, children))
        case Operator(type=1, children=children):
            return prod(map(eval_packet, children))
        case Operator(type=2, children=children):
            return min(map(eval_packet, children))
        case Operator(type=3, children=children):
            return max(map(eval_packet, children))
        case Operator(type=5, children=[lhs, rhs]):
            return int(eval_packet(lhs) > eval_packet(rhs))
        case Operator(type=6, children=[lhs, rhs]):
            return int(eval_packet(lhs) < eval_packet(rhs))
        case Operator(type=7, children=[lhs, rhs]):
            return int(eval_packet(lhs) == eval_packet(rhs))


def part1(lines):
    """
    >>> part1(["8A004A801A8002F478"])
    16
    >>> part1(["620080001611562C8802118E34"])
    12
    >>> part1(["C0015000016115A2E0802F182340"])
    23
    >>> part1(["A0016C880162017C3686B18A3D4780"])
    31
    """
    return sum_versions(parse_packet(hex_to_bits(iter(lines[0].strip()))))


def part2(lines):
    """
    >>> part2(["C200B40A82"])
    3
    >>> part2(["04005AC33890"])
    54
    >>> part2(["880086C3E88112"])
    7
    >>> part2(["CE00C43D881120"])
    9
    >>> part2(["D8005AC2A8F0"])
    1
    >>> part2(["F600BC2D8F"])
    0
    >>> part2(["9C005AC2F8F0"])
    0
    >>> part2(["9C0141080250320F1802104A08"])
    1
    """
    return eval_packet(parse_packet(hex_to_bits(iter(lines[0].strip()))))


parts = (part1, part2)
