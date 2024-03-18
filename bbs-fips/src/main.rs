use gcd::Gcd;
use rand::Rng;
use std::{time::{SystemTime, UNIX_EPOCH}, collections::HashMap};

fn check_input(p: u32, q: u32) {
    let check_mod = |n: u32| n % 4 == 3;
    assert!(primal::is_prime(p.into()));
    assert!(primal::is_prime(q.into()));
    assert!(check_mod(p));
    assert!(check_mod(q));
}

struct BBS {
    n: u128,
    seed: u128,
}

impl BBS {
    pub fn new(p: u32, q: u32) -> Self {
        check_input(p, q);
        let n: u128 = (p * q).into();
        let seed = Self::find_seed(n);
        Self { seed, n }
    }
    pub fn next(&mut self) -> bool {
        let (n, x) = (self.n, self.seed);
        let x = (x * x) % n;
        self.seed = x;
        x % 2 != 0
    }

    /**
    Find such X that gcd(N, X) == 1
     **/
    fn find_seed(n: u128) -> u128 {
        loop {
            let x = rand::thread_rng().gen_range(1000..=9999) as u128;
            dbg!(x);
            if x.gcd(n) == 1 {
                return x;
            }
        }
    }
}

fn main() {
    println!("Run the tests!");
}

const SIZE: usize = 20_000;

fn get_bits() -> Vec<bool> {
    let p: u32 = 7927;
    let q: u32 = 7331;
    let mut gen = BBS::new(p, q);
    (0..SIZE).map(|_| gen.next()).collect()
}

#[test]
fn single_bits_test() {
    let bits = get_bits();
    let ones = bits.iter().filter(|&&b| b == true).count();
    assert!(9725 < ones && ones < 10275)
}

fn series_of(arr: &[bool], kind: bool) -> Vec<i32> {
    let mut current_length = 0;
    let mut counts = vec![0; 7];
    for bit in arr {
        if bit == &kind {
            current_length += 1;
        } else {
            if current_length > 0 {
                counts[current_length.min(6)] += 1;
                current_length = 0;
            }
        }
    }
    if current_length > 0 {
        counts[current_length.min(6)] += 1;
    }
    counts
}
fn long_series_of(arr: &[bool], kind: bool) -> bool {
    let mut current_length = 0;
    for bit in arr {
        if bit == &kind {
            current_length += 1;
        } else {
            if current_length >= 26 {
                return false
            }
        }
    }
    if current_length >= 26 {
        return false;
    }
    true
}

#[test]
fn series_test() {
    let permitted_ranges = HashMap::<usize, _>::from([
        (1, 2315..=2685),
        (2, 1114..=1386),
        (3, 527..=723),
        (4, 240..=384),
        (5, 103..=209),
        (6, 103..=209),
    ]);
    let bits = get_bits();
    let lengths = 1..=6;

    let counts0 = series_of(&bits, true);
    let counts1 = series_of(&bits, false);

    for len in lengths {
        let series0 = counts0[len];
        assert!(permitted_ranges.get(&len).unwrap().contains(&series0));
        let series1 = counts1[len];
        assert!(permitted_ranges.get(&len).unwrap().contains(&series1));
    }
}

#[test]
fn long_series_test() {
    let bits = get_bits();
    let series0 = long_series_of(&bits, true);
    let series1 = long_series_of(&bits, false);
    assert!(!series0 && !series1);
}

fn convert(bits: &[bool]) -> u8 {
    let mut result: u8 = 0;
    bits.iter().for_each(|&bit| {
        result <<= 1;
        result ^= bit as u8;
    });
    result
}

#[test]
fn poker_test() {
    let bits = get_bits();
    let mut counts = HashMap::<u8, usize>::with_capacity(16);
    let chunk_size = SIZE / 5_000;
    let numbers = bits.chunks(chunk_size).map(|bits| convert(bits));
    for n in numbers {
        *counts.entry(n).or_insert(0) += 1;
    }
    let sum = counts.iter().fold(1, |sum, (_, count)| sum + (count * count) as u32) as f32;
    let x = 16.0/5000.0 * sum - 5000.0;
    assert!(2.16 < x && x < 46.17);
}
