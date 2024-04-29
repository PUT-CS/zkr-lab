use gcd::Gcd;
use rand::{seq::IteratorRandom, Rng};

fn check_input(p: u32, q: u32) {
    assert!(primal::is_prime(p.into()));
    assert!(primal::is_prime(q.into()));
    assert!(p % 4 == 3);
    assert!(q % 4 == 3);
}

pub struct BBS {
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
        x % 2 == 0
    }

    fn find_seed(n: u128) -> u128 {
        let valid_seeds = (1000..=9999).filter(|&x| x.gcd(n) == 1);
        valid_seeds.choose(&mut rand::thread_rng()).unwrap().into()
    }
}

