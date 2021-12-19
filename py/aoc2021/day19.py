import itertools
from collections import defaultdict

SAMPLE_INPUT = [
    "--- scanner 0 ---",
    "404,-588,-901",
    "528,-643,409",
    "-838,591,734",
    "390,-675,-793",
    "-537,-823,-458",
    "-485,-357,347",
    "-345,-311,381",
    "-661,-816,-575",
    "-876,649,763",
    "-618,-824,-621",
    "553,345,-567",
    "474,580,667",
    "-447,-329,318",
    "-584,868,-557",
    "544,-627,-890",
    "564,392,-477",
    "455,729,728",
    "-892,524,684",
    "-689,845,-530",
    "423,-701,434",
    "7,-33,-71",
    "630,319,-379",
    "443,580,662",
    "-789,900,-551",
    "459,-707,401",
    "",
    "--- scanner 1 ---",
    "686,422,578",
    "605,423,415",
    "515,917,-361",
    "-336,658,858",
    "95,138,22",
    "-476,619,847",
    "-340,-569,-846",
    "567,-361,727",
    "-460,603,-452",
    "669,-402,600",
    "729,430,532",
    "-500,-761,534",
    "-322,571,750",
    "-466,-666,-811",
    "-429,-592,574",
    "-355,545,-477",
    "703,-491,-529",
    "-328,-685,520",
    "413,935,-424",
    "-391,539,-444",
    "586,-435,557",
    "-364,-763,-893",
    "807,-499,-711",
    "755,-354,-619",
    "553,889,-390",
    "",
    "--- scanner 2 ---",
    "649,640,665",
    "682,-795,504",
    "-784,533,-524",
    "-644,584,-595",
    "-588,-843,648",
    "-30,6,44",
    "-674,560,763",
    "500,723,-460",
    "609,671,-379",
    "-555,-800,653",
    "-675,-892,-343",
    "697,-426,-610",
    "578,704,681",
    "493,664,-388",
    "-671,-858,530",
    "-667,343,800",
    "571,-461,-707",
    "-138,-166,112",
    "-889,563,-600",
    "646,-828,498",
    "640,759,510",
    "-630,509,768",
    "-681,-892,-333",
    "673,-379,-804",
    "-742,-814,-386",
    "577,-820,562",
    "",
    "--- scanner 3 ---",
    "-589,542,597",
    "605,-692,669",
    "-500,565,-823",
    "-660,373,557",
    "-458,-679,-417",
    "-488,449,543",
    "-626,468,-788",
    "338,-750,-386",
    "528,-832,-391",
    "562,-778,733",
    "-938,-730,414",
    "543,643,-506",
    "-524,371,-870",
    "407,773,750",
    "-104,29,83",
    "378,-903,-323",
    "-778,-728,485",
    "426,699,580",
    "-438,-605,-362",
    "-469,-447,-387",
    "509,732,623",
    "647,635,-688",
    "-868,-804,481",
    "614,-800,639",
    "595,780,-596",
    "",
    "--- scanner 4 ---",
    "727,592,562",
    "-293,-554,779",
    "441,611,-461",
    "-714,465,-776",
    "-743,427,-804",
    "-660,-479,-426",
    "832,-632,460",
    "927,-485,-438",
    "408,393,-506",
    "466,436,-512",
    "110,16,151",
    "-258,-428,682",
    "-393,719,612",
    "-211,-452,876",
    "808,-476,-593",
    "-575,615,604",
    "-485,667,467",
    "-680,325,-822",
    "-627,-443,-432",
    "872,-547,-609",
    "833,512,582",
    "807,604,487",
    "839,-516,451",
    "891,-625,532",
    "-652,-548,-490",
    "30,-46,-14",
]


def parse(lines):
    for ok, group in itertools.groupby((line.strip() for line in lines), bool):
        if not ok:
            continue
        next(group)
        yield [tuple(map(int, line.split(","))) for line in group]


def all_rotations(items):
    """
    >>> sorted("".join(("+" if b else "-") + a for b, a in rotation) for rotation in all_rotations("xy"))
    ['+x+y', '+y-x', '-x-y', '-y+x']
    >>> sorted("".join(('+' if b else '-') + a for b, a in rotation) for rotation in all_rotations("xyz"))
    ['+x+y+z', '+x+z-y', '+x-y-z', '+x-z+y', '+y+x-z', '+y+z+x', '+y-x+z', '+y-z-x', '+z+x+y', '+z+y-x', '+z-x-y', '+z-y+x', '-x+y-z', '-x+z+y', '-x-y+z', '-x-z-y', '-y+x+z', '-y+z-x', '-y-x-z', '-y-z+x', '-z+x-y', '-z+y+x', '-z-x+y', '-z-y-x']
    """
    for indices in itertools.permutations(enumerate(items)):
        parity, seen = False, 0
        for i, (j, _) in enumerate(indices):
            if seen & 1 << i:
                continue
            parity = not parity
            while not seen & 1 << j:
                seen = seen | 1 << j
                j, parity = indices[j][0], not parity
        for bits in range(1 << len(indices)):
            if bool(bits.bit_count() % 2) == parity:
                yield tuple(
                    (not (bits & 1 << i), item) for i, (_, item) in enumerate(indices)
                )


def solve(lines):
    """
    >>> solve(SAMPLE_INPUT)
    (79, 3621)
    """
    scanners = list(parse(lines))
    deltas = defaultdict(list)
    for i, scanner in itertools.islice(enumerate(scanners), 1, None):
        delta = [tuple(x - y for x, y in zip(a, b)) for a in scanner for b in scanner]
        for rotation in all_rotations(range(len(scanner[0]))):
            deltas[
                frozenset(
                    tuple(v[j] if sign else -v[j] for sign, j in rotation)
                    for v in delta
                )
            ].append((i, rotation))
    beacons = set(scanners[0])
    positions = [(0,) * len(scanners[0][0])]
    remaining = set(range(1, len(scanners)))
    while remaining:
        delta = set(
            tuple(x - y for x, y in zip(a, b)) for a in beacons for b in beacons
        )
        found = False
        for _, value in sorted(
            deltas.items(), key=lambda item: len(delta & item[0]), reverse=True
        ):
            for i, rotation in value:
                if i not in remaining:
                    continue
                scanner = [
                    tuple(v[j] if sign else -v[j] for sign, j in rotation)
                    for v in scanners[i]
                ]
                for position in set(
                    tuple(x - y for x, y in zip(a, b)) for a in beacons for b in scanner
                ):
                    points = set(
                        tuple(x + y for x, y in zip(position, v)) for v in scanner
                    )
                    if len(beacons & points) >= 12:
                        found = True
                        beacons |= points
                        positions.append(position)
                        remaining.remove(i)
                        break
                if found:
                    break
            if found:
                break
        if not found:
            raise AssertionError()
    return len(beacons), max(
        sum(abs(x - y) for x, y in zip(a, b)) for a in positions for b in positions
    )


parts = (solve,)
