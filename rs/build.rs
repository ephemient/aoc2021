use build_const::ConstWriter;
use std::collections::BTreeMap;
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Error, ErrorKind};
use std::iter::FromIterator;
use std::path::Path;

const DAY6_STEP: [[u64; 9]; 9] = [
    [0, 1, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0],
];

fn day6_mul<const I: usize, const J: usize, const K: usize>(
    x: &[[u64; K]; I],
    y: &[[u64; J]; K],
) -> [[u64; J]; I] {
    let mut z = [[0; J]; I];
    for (i, row) in z.iter_mut().enumerate() {
        for (j, cell) in row.iter_mut().enumerate() {
            *cell = (0..K).map(|k| x[i][k] * y[k][j]).sum();
        }
    }
    z
}

fn day6_pow<const I: usize>(base: &[[u64; I]; I], e: usize) -> [[u64; I]; I] {
    if e <= 1 {
        return *base;
    }
    let double = day6_pow(&day6_mul(base, base), e / 2);
    if e % 2 == 0 {
        double
    } else {
        day6_mul(&double, base)
    }
}

fn day6_lut(times: usize) -> [u64; 9] {
    let mat = day6_pow(&DAY6_STEP, times);
    let mut lut = [0; 9];
    for (i, x) in lut.iter_mut().enumerate() {
        *x = mat.iter().map(|row| row[i]).sum();
    }
    lut
}

fn main() -> io::Result<()> {
    let days = BTreeMap::from_iter(
        fs::read_dir(
            Path::new(
                &env::var("CARGO_MANIFEST_DIR").map_err(|e| Error::new(ErrorKind::Other, e))?,
            )
            .parent()
            .ok_or_else(|| Error::new(ErrorKind::Other, "no parent directory"))?,
        )?
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| path.is_file())
        .filter_map(|path| {
            let name = path.file_name()?.to_str()?;
            if !name.starts_with("day") || !name.ends_with(".txt") {
                return None;
            }
            Some((
                name[3..name.len() - 4].parse::<u32>().ok()?,
                path.to_owned(),
            ))
        }),
    );

    let mut consts = ConstWriter::for_build("aoc2021")?.finish_dependencies();
    for (day, path) in days {
        let lines = BufReader::new(File::open(path)?)
            .lines()
            .collect::<io::Result<Vec<String>>>()?;
        consts.add_value_raw(&format!("DAY{}", day), "&[&str]", &format!("&{:?}", lines));
    }
    consts.finish();

    let mut consts = ConstWriter::for_build("libaoc2021_day6")?.finish_dependencies();
    for times in [80, 256] {
        consts.add_value_raw(
            &format!("LUT_{}", times),
            "[usize; 9]",
            &format!("{:?}", &day6_lut(times)),
        );
    }
    consts.finish();

    Ok(())
}
