import random
from sympy import randprime

# random polynomial, max power = threshold -1
def generate_polynomial(secret, threshold, prime):
    return [secret, *[random.randint(1, prime - 1) for _ in range(1,threshold)]]

# polynomial value for given `x`
def eval_polynomial(coefficients, x, prime):
    result = 0
    for coefficient in reversed(coefficients):
        result = (result * x + coefficient) % prime
    return result

# generate shares from a polynomial
def get_shares(secret, num_shares, threshold):
    prime = randprime(secret + num_shares + threshold, 2 * (secret + num_shares + threshold))
    coefficients = generate_polynomial(secret, threshold, prime)
    shares = [(x, eval_polynomial(coefficients, x, prime)) for x in range(1, num_shares + 1)]
    return shares, prime

# reconstruct the secret from shares
def join_shares(shares, prime):
    out = 0
    for j, (xj, yj) in enumerate(shares):
        numerator, denominator = 1, 1
        for i, (xi, _) in enumerate(shares):
            if i != j:
                numerator = (numerator * (-xi)) % prime
                denominator = (denominator * (xj - xi)) % prime
        lagrange = (yj * numerator * pow(denominator, -1, prime)) % prime
        out = (out + lagrange) % prime
    return out

def main():
    secret = random.randint(5, 100)
    share_count = random.randint(10,15)
    threshold = random.randint(8, share_count)

    print("secret: ", secret)

    shares, prime = get_shares(secret, share_count, threshold)
    print("shares:", shares)

    reconstructed_secret = join_shares(shares[:threshold], prime)
    print("deciphered secret:", reconstructed_secret)

main()