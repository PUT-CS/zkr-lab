import hashlib
import timeit
import functools
import random
import string

def rand_str(length):
    characters = string.ascii_letters + string.digits + string.punctuation
    return ''.join(random.choice(characters) for _ in range(length))

num_of_inputs = 20
inputs = [[rand_str(n).encode() for i in range(1, num_of_inputs)] for n in [10, 100, 1000, 10_000, 100_000]]
algos = ['md5', 'sha1', 'sha224', 'sha256', 'sha384', 'sha512', 'sha3_224', 'sha3_256', 'sha3_384', 'sha3_512']
hashers = [hashlib.new(algo) for algo in algos]


def benchmarks():
    def bench(hasher, data: list[str]):
        for input in data:
            hasher.update(input)
            hasher.hexdigest()

    hash_times = {}
    for input in inputs:
        for hasher in hashers:
            fn = functools.partial(bench, hasher, input)
            duration = timeit.timeit(fn, number=10)
            hash_times[hasher.name] = duration

    hash_lengths = { hasher.name: len(''.join(format(ord(i), '08b') for i in hasher.hexdigest())) / 2 for hasher in hashers}

    import matplotlib.pyplot as plt
    plt.bar(*zip(*sorted(hash_lengths.items(), key=lambda x: x[1])))
    plt.title('Hash lengths')
    plt.show()

    plt.plot(*zip(*sorted(hash_times.items(), key=lambda x: x[1])))
    plt.title('Hashing time')
    plt.show()
        


def str_to_bits(string) -> list[chr]:
    return ''.join(format(ord(i), '08b') for i in string)
        
def collisions():
    print("Checking for collisions in the first 12 bits of the hash...")
    to_check = 12
    tests = 10000
    collision_count = 0

    base_hash = hashlib.sha256(rand_str(30).encode()).hexdigest()
    base_hash_bits = str_to_bits(base_hash)

    for i in range(tests):
        s = rand_str(30)
        s_hash = hashlib.sha256(s.encode()).hexdigest()
        s_hash_bits = str_to_bits(s_hash)
        if s_hash_bits[:to_check] == base_hash_bits[:to_check]:
            collision_count += 1

    print("Collision count:", collision_count)
    print("in", tests, "tests")
    print("Collision probability:", collision_count / tests)

def input_pairs(length):
    def even_letter_code():
        letter_codes = [i for i in range(99, 122) if i % 2 == 0]
        return random.choice(letter_codes)
        
    string = rand_str(5)
    letter = even_letter_code()
    a = string + chr(letter)
    b = string + chr(letter + 1)
    return (a, b)

def generate_mutations(string):
    swap = lambda old, char: old[:i] + char + old[i+1:]
    mutations = []
    for i, bit in enumerate(string):
        if bit == "0":
            mutations.append(swap(string, "1"))
        else:
            mutations.append(swap(string, "0"))
    return mutations
        
def sac():
    text1 = hashlib.sha256("ala ma c".encode()).hexdigest()
    text2 = hashlib.sha256("ala ma d".encode()).hexdigest()
    
    hashed_text1 = bin(int(text1, 16)).removeprefix("0b").zfill(256)
    hashed_text2 = bin(int(text2, 16)).removeprefix("0b").zfill(256)
    count = sum([1 if hashed_text1[x] == hashed_text2[x] else 0 for x in range(len(hashed_text1))])

    result = count / len(hashed_text1)
    print("SAC:", round(result, 6))
        
def main():
    test_md5 = hashlib.md5("Kot".encode()).hexdigest()
    #print(test_md5)
    #collisions()
    sac()
    #benchmarks()
        
main()
