#[cfg(test)]
mod test {
    use bbs_fips::BBS;
    use lazy_static::lazy_static;
    use std::collections::HashMap;
    const SIZE: usize = 20_000;

    lazy_static! {
        static ref BITS: Vec<bool> = get_bits();
    }

    #[test]
    fn single_bits_test() {
        let ones = BITS.iter().filter(|&&b| b == true).count();
        assert!(9725 < ones && ones < 10275)
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
        let lengths = 1..=6;

        let counts0 = series(&BITS, true);
        let counts1 = series(&BITS, false);

        for len in lengths {
            let series0 = counts0[len];
            assert!(permitted_ranges.get(&len).unwrap().contains(&series0));
            let series1 = counts1[len];
            assert!(permitted_ranges.get(&len).unwrap().contains(&series1));
        }
    }

    #[test]
    fn long_series_test() {
        let series0 = long_series(&BITS, true);
        let series1 = long_series(&BITS, false);
        assert!(!series0 && !series1);
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

    fn long_series(arr: &[bool], kind: bool) -> bool {
        let mut current_length = 0;
        for bit in arr {
            if bit == &kind {
                current_length += 1;
            } else {
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

    fn get_bits() -> Vec<bool> {
        // let p: u32 = 7927;
        // let q: u32 = 7331;
        let p: u32 = 1999;
        let q: u32 = 8243;
        let mut gen = BBS::new(p, q);
        (0..SIZE).map(|_| gen.next()).collect()
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
