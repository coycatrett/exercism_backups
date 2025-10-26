#include "sieve.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
    if (limit < 2) return 0;

    // total number of odd integers between n and 3 inclusive
    int sieve_length = ((limit - 2) >> 1) + (limit & 1);

    // initialize sieve with all false
    bool* sieve = calloc(sieve_length, sizeof(bool));

    // only need to start at odd numbers between 3 and sqrt(n)
    int terminate = floor(sqrt(limit));

    // Sieve of Erastothenes
    for (int p = 3; p <= terminate; p += 2) {
        int idx = ((p - 1) >> 1) - 1;

        // if number is unseen (false)
        if (!sieve[idx]) {
            for (int j = ((p * p - 1) >> 1) - 1; j < sieve_length; j += p) {
                sieve[j] = true;
            }
        }
    }

    // Collect primes
    uint32_t p = 1;
    primes[0] = 2;
    for (int j = 0; j < sieve_length; j++) {
        // primes are all remaining false elements in the Sieve
        if (!sieve[j] && p < max_primes) {
            primes[p++] = 2 * j + 3;
        }
    }
    free(sieve);
    return p;
}