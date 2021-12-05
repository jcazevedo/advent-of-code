#include <bits/stdc++.h>

using namespace std;

long long getFuelPart1(const vector<long long> &masses) {
  long long tot = 0;
  for (long long mass : masses)
    tot += (mass / 3) - 2;
  return tot;
}

long long getModuleFuel(long long mass, map<long long, long long>& cache) {
  if (cache.find(mass) == cache.end()) {
    long long next = max(mass / 3 - 2, 0ll);
    long long fuel = next + (next ? getModuleFuel(next, cache) : 0);
    cache[mass] = fuel;
  }
  return cache[mass];
}

long long getFuelPart2(const vector<long long> &masses) {
  map<long long, long long> cache;
  long long tot = 0;
  for (long long mass : masses)
    tot += getModuleFuel(mass, cache);
  return tot;
}

int main() {
  ifstream fin("day01.in");
  long long v;
  vector<long long> masses;
  while (fin >> v)
    masses.push_back(v);
  fin.close();
  cout << "Part 1: " << getFuelPart1(masses) << endl;
  cout << "Part 2: " << getFuelPart2(masses) << endl;
  return 0;
}
