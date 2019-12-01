#include <bits/stdc++.h>

using namespace std;

int main() {
  ifstream fin("day01.in");
  long long tot = 0, v;
  while (fin >> v) {
    tot += (v / 3) - 2;
  }
  cout << "Part 1: " << tot << endl;
  return 0;
}
