from math import gcd

def primRoots(modulo):
    coprime_set = {num for num in range(1, modulo) if gcd(num, modulo) == 1}
    print(coprime_set)
    return [g for g in range(1, modulo) if coprime_set == {
        pow(g, powers, modulo) for powers in range(1, modulo)
    }]

print(primRoots(17))
