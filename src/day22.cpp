#include <bits/stdc++.h>

using namespace std;

#define N_CARDS 10007
#define N_CARDS_L 119315717514047ll
#define MULT 101741582076661ll

int stackIdx(int orig, int N) {
  return N - orig - 1;
}

int cutIdx(int orig, int N, int cut) {
  if (cut < 0)
    return cutIdx(orig, N, N + cut);
  int diff = N - cut;
  return (orig + diff) % N;
}

int dealIdx(int orig, int N, int inc) {
  return (orig * inc) % N;
}

int main() {
  ifstream fin("day22.in");
  string str;
  int pos = 2019;
  while (getline(fin, str)) {
    if (str.rfind("deal into new stack", 0) == 0) {
      pos = stackIdx(pos, N_CARDS);
    } else if (str.rfind("deal with increment", 0) == 0) {
      istringstream ss(str);
      string tmp;
      int inc;
      ss >> tmp >> tmp >> tmp >> inc;
      pos = dealIdx(pos, N_CARDS, inc);
    } else if (str.rfind("cut", 0) == 0) {
      istringstream ss(str);
      string tmp;
      int inc;
      ss >> tmp >> inc;
      pos = cutIdx(pos, N_CARDS, inc);
    }
  }
  cout << "Part 1: " << pos << endl;
  return 0;
}
