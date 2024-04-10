use gcd::Gcd;
use rand::Rng;

fn check_input(p: u32, q: u32) {
    let check_mod = |n: u32| return n % 4 == 3;
    assert!(primal::is_prime(p.into()));
    assert!(primal::is_prime(q.into()));
    assert!(check_mod(p));
    assert!(check_mod(q));
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
        x % 2 != 0
    }

    fn find_seed(n: u128) -> u128 {
        loop {
            let x = rand::thread_rng().gen_range(1000..=9999) as u128;
            if x.gcd(n) == 1 {
                return x;
            }
        }
    }
}

