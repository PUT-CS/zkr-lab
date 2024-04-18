#[cfg(test)]
mod test {
    use bbs_fips::BBS;
    use lazy_static::lazy_static;
    use rand::seq::SliceRandom;
    use std::{collections::HashMap, ops::Sub};
    const SIZE: usize = 20_000;

    lazy_static! {
        static ref BITS: Vec<bool> = get_bits();
        static ref PRIMES: Vec<u32> = get_primes();
    }

    fn get_primes() -> Vec<u32> {
        (1000..=10000)
            .filter(|&n| primal::is_prime(n) && n % 4 == 3)
            .map(|n| n as u32)
            .collect()
    }

    fn rand_primes() -> (u32, u32) {
        let p = PRIMES.choose(&mut rand::thread_rng()).unwrap();
        let q = PRIMES.choose(&mut rand::thread_rng()).unwrap();
        (*p, *q)
    }

    fn get_bits() -> Vec<bool> {
        let (p, q) = rand_primes();
        let mut gen = BBS::new(p, q);
        (0..SIZE).map(|_| gen.next()).collect()
    }

    #[test]
    fn single_bits_test() {
        let ones = BITS.iter().filter(|&&b| b == true).count();
        assert!(9725 < ones && ones < 10275)
    }

    #[test]
    fn series_test() {
        let permitted_ranges = HashMap::from([
            (1, 2315..=2685),
            (2, 1114..=1386),
            (3, 527..=723),
            (4, 240..=384),
            (5, 103..=209),
            (6, 103..=209),
        ]);

        let counts0 = series(&BITS, true);
        let counts1 = series(&BITS, false);

        for (len, range) in permitted_ranges {
            let series0 = counts0[len - 1];
            let series1 = counts1[len - 1];
            assert!(range.contains(&series0));
            assert!(range.contains(&series1));
        }
    }

    #[test]
    fn long_series_test() {
        let series0 = long_series(&BITS, true);
        let series1 = long_series(&BITS, false);
        assert!(series0 && series1);
    }

    #[test]
    fn poker_test() {
        let mut counts = HashMap::<u8, usize>::with_capacity(16);
        let chunk_size = SIZE / 5_000;
        let numbers = BITS.chunks(chunk_size).map(|b| convert(b));
        for n in numbers {
            *counts.entry(n).or_insert(0) += 1;
        }
        let sum = counts
            .iter()
            .fold(1, |sum, (_, count)| sum + count.pow(2) as u32) as f32;
        let x = 16.0 / 5000.0 * sum - 5000.0;
        assert!(2.16 < x && x < 46.17);
    }

    fn series(arr: &[bool], kind: bool) -> Vec<i32> {
        let mut current_length: usize = 0;
        let mut counts = vec![0; 6];
        for bit in arr {
            if bit == &kind {
                current_length += 1;
            } else {
                if current_length > 0 {
                    counts[current_length.sub(1).min(5)] += 1;
                    current_length = 0;
                }
            }
        }
        if current_length > 0 {
            counts[current_length.sub(1).min(5)] += 1;
        }
        counts
    }

    fn long_series(arr: &[bool], kind: bool) -> bool {
        let mut current_length = 0;
        for bit in arr {
            if bit == &kind {
                current_length += 1;
                dbg!(current_length);
            } else {
                current_length = 0;
                if current_length >= 26 {
                    return false;
                }
            }
        }
        if current_length >= 26 {
            return false;
        }
        true
    }

    fn convert(bits: &[bool]) -> u8 {
        let mut result: u8 = 0;
        bits.iter().for_each(|&bit| {
            result <<= 1;
            result ^= bit as u8;
        });
        result
    }
}
