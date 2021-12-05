#include <bits/stdc++.h>

using namespace std;

bool good(int value, bool repG) {
  bool hasPair = false;
  int prev = 10, rep = 1;
  while (value) {
    int next = value % 10;
    if (next > prev)
      return false;
    if (next == prev) {
      if (!repG)
        hasPair = true;
      rep++;
    } else {
      if (repG && rep == 2)
        hasPair = true;
      rep = 1;
    }
    prev = next;
    value /= 10;
  }
  if (rep == 2)
    hasPair = true;
  return hasPair;
}

int countPasswords(int lo, int hi, bool rep = false) {
  int tot = 0;
  for (int i = lo; i <= hi; ++i)
    tot += good(i, rep);
  return tot;
}

int main() {
  FILE* fp = fopen("day04.in", "r");
  int lo, hi;
  fscanf(fp, "%d-%d", &lo, &hi);
  fclose(fp);
  cout << "Part 1: " << countPasswords(lo, hi) << endl;
  cout << "Part 2: " << countPasswords(lo, hi, true) << endl;
  return 0;
}
