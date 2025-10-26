#include "sieve.h"
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
    if (limit < 2) return 0;

    // Total number of odd integers between n and 3 inclusive
    int sieve_length = ((limit - 2) >> 1) + (limit & 1);

    // Initialize sieve with all false
    bool* sieve = calloc(sieve_length, sizeof(bool));

    // Only need to look at odd numbers between 3 and sqrt(n)
    int terminate = floor(sqrt(limit));

    // Sieve of Erastothenes
    for (int p = 3; p <= terminate; p += 2) {
        int idx = ((p - 1) >> 1) - 1;

        // If p is unseen (false)
        if (!sieve[idx]) {
            // Mark multiples of p  
            for (int j = ((p * p - 1) >> 1) - 1; j < sieve_length; j += p) {
                // Can start at index of p^2 since any prime multiple of p less than p^2
                // will have factors strictly less than p and be marked seen by another prime 
                sieve[j] = true;
            }
        }
    }

    // Collect primes
    uint32_t p = 1;
    primes[0] = 2;
    for (int j = 0; j < sieve_length; j++) {
        // Primes are all remaining false elements in the Sieve
        if (!sieve[j] && p < max_primes) {
            primes[p++] = 2 * j + 3;
        }
    }
    
    free(sieve);
    return p;
}