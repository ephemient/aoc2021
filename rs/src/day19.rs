use super::util;
use std::collections::HashSet;
use std::error::Error;
use std::ops::Neg;
use std::str::FromStr;

fn parse<'a, I, S, Item, const N: usize>(
    lines: I,
) -> Result<Vec<Vec<[Item; N]>>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
    Item: Copy + Default + FromStr,
    <Item as FromStr>::Err: 'static + Error + Send + Sync,
{
    let mut scanners = Vec::<Vec<[Item; N]>>::new();
    let mut scanner = Vec::<[Item; N]>::new();
    for line in lines {
        let line = line.as_ref();
        if line.is_empty() {
            if !scanner.is_empty() {
                scanners.push(scanner);
                scanner = Vec::new();
            }
            continue;
        }
        if line.starts_with("--- scanner ") && line.ends_with(" ---") {
            continue;
        }
        let mut data = [Item::default(); N];
        for (i, item) in line.split(',').take(N).enumerate() {
            data[i] = item.parse()?;
        }
        scanner.push(data);
    }
    if !scanner.is_empty() {
        scanners.push(scanner);
    }
    Ok(scanners)
}

#[derive(Copy, Clone, Debug)]
enum Rotation<const N: usize> {
    New,
    State {
        permutation: [usize; N],
        bits: usize,
    },
}

impl<const N: usize> Iterator for Rotation<N> {
    type Item = [(bool, usize); N];
    fn next(&mut self) -> Option<Self::Item> {
        let (permutation, bits) = match self {
            Rotation::New => {
                let mut permutations = [0; N];
                for (i, x) in permutations.iter_mut().enumerate().skip(1) {
                    *x = i;
                }
                (permutations, 0)
            }
            Rotation::State { permutation, bits } => {
                let parity = bits.count_ones() as usize & 1;
                let mut new_bits = (*bits & !1) + 2;
                new_bits |= new_bits.count_ones() as usize ^ parity & 1;
                if new_bits < 1 << N {
                    (*permutation, new_bits)
                } else if let Some(i) = (0..N - 1)
                    .rev()
                    .find(|&i| permutation[i] < permutation[i + 1])
                {
                    let j = (i + 1..N)
                        .filter(|j| permutation[i] < permutation[*j])
                        .min_by_key(|j| permutation[*j])
                        .unwrap();
                    permutation.swap(i, j);
                    permutation[i + 1..].reverse();
                    (*permutation, parity ^ i ^ !N & 1)
                } else {
                    return None;
                }
            }
        };
        let mut ret = [(false, 0); N];
        for i in 0..N {
            ret[i] = (bits & 1 << i != 0, permutation[i]);
        }
        *self = Rotation::State { permutation, bits };
        Some(ret)
    }
}

fn apply_rotation<Item, const N: usize>(
    point: &[Item; N],
    rotation: &[(bool, usize); N],
) -> [Item; N]
where
    Item: Copy + Default + Neg<Output = Item>,
{
    let mut output = [Item::default(); N];
    for (j, &(sign, i)) in rotation.iter().enumerate() {
        output[j] = if sign { -point[i] } else { point[i] };
    }
    output
}

pub fn solve<'a, I, S>(lines: I) -> Result<(usize, i32), Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut scanners: Vec<Vec<[i32; 3]>> = parse(lines)?;
    let mut beacons: HashSet<_> = scanners.pop().ok_or(util::Error)?.drain(..).collect();
    let mut deltas: Vec<_> = scanners
        .iter()
        .enumerate()
        .flat_map(|(i, scanner)| {
            let delta = scanner
                .iter()
                .flat_map(|x| {
                    scanner
                        .iter()
                        .map(|y| [x[0] - y[0], x[1] - y[1], x[2] - y[2]])
                })
                .collect::<HashSet<_>>();
            Rotation::<3>::New.map(move |rotation| {
                (
                    delta
                        .iter()
                        .map(|point| apply_rotation(point, &rotation))
                        .collect::<Vec<_>>(),
                    (i, rotation),
                )
            })
        })
        .collect();
    let mut positions = vec![[0i32; 3]];
    'outer: while !deltas.is_empty() {
        let delta = beacons
            .iter()
            .flat_map(|x| {
                beacons
                    .iter()
                    .map(|y| [x[0] - y[0], x[1] - y[1], x[2] - y[2]])
            })
            .collect::<HashSet<_>>();
        deltas.sort_unstable_by_key(|(key, _)| {
            key.iter().cloned().filter(|v| delta.contains(v)).count()
        });
        for (_, (i, rotation)) in deltas.iter().rev() {
            let scanner = scanners[*i]
                .iter()
                .map(|point| apply_rotation(point, rotation))
                .collect::<Vec<_>>();
            if let Some((position, points)) = beacons
                .iter()
                .flat_map(|x| {
                    scanner
                        .iter()
                        .map(|y| [x[0] - y[0], x[1] - y[1], x[2] - y[2]])
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|x| {
                    (
                        x,
                        scanner
                            .iter()
                            .map(|y| [x[0] + y[0], x[1] + y[1], x[2] + y[2]])
                            .collect::<Vec<_>>(),
                    )
                })
                .find(|(_, points)| {
                    points
                        .iter()
                        .cloned()
                        .filter(|point| beacons.contains(point))
                        .count()
                        >= 12
                })
            {
                beacons.extend(points);
                positions.push(position);
                let i = *i;
                deltas.retain(|(_, (j, _))| i != *j);
                continue 'outer;
            }
        }
        return Err(util::Error.into());
    }
    Ok((
        beacons.len(),
        positions
            .iter()
            .flat_map(|x| {
                positions
                    .iter()
                    .map(|y| (x[0] - y[0]).abs() + (x[1] - y[1]).abs() + (x[2] - y[2]).abs())
            })
            .max()
            .unwrap_or(0),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
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
    ];

    #[test]
    fn solve_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!((79, 3621), solve(EXAMPLE)?);
        Ok(())
    }
}
