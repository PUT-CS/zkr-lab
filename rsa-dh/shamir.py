import random
import math

def is_prime(number):
    for i in range(2, math.ceil(math.sqrt(number))):
        if number % i == 0:
            return False
    return True

rand_in_range = lambda a, b: random.randrange(a, b)

PRIMES = [i for i in range(100, 1000) if is_prime(i)]

def get_P(s, n):
    valid = [p for p in PRIMES if p > s and p > n]
    return random.choice(valid)

def shamir(secret, n, t):
    P = get_P(secret, n)
    a = [random.randrange(10, 50) for _ in range(t - 1)]
    print(a)

def main():
    secret = 954
    fragments = 4
    threshold = 3
    shamir(secret, fragments, threshold)

main()
