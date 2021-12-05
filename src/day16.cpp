#include <bits/stdc++.h>

using namespace std;

typedef long long ll;

// This only works for my input since the offset is on the second half of the number.
string fft1(string s, int n) {
  string str = "";
  for (int i = 0; i < 10000; ++i)
    str += s;
  int N = str.size();
  istringstream ss(str.substr(0, 7));
  int offset;
  ss >> offset;
  for (int i = 0; i < n; ++i) {
    ll sum = 0;
    for (int j = offset; j < N; ++j)
      sum += (str[j] - '0');
    for (int j = offset; j < N; ++j) {
      int res = abs(sum) % 10;
      sum -= (str[j] - '0');
      str[j] = res + '0';
    }
  }
  return str.substr(offset, 8);
}

#define PATSIZE 4

int pat[] = {0, 1, 0, -1};

string fft(string s, int n) {
  if (n == 0)
    return s;
  int N = s.size();
  for (int i = 0; i < N; ++i) {
    int rep = i + 1;
    int idx = 0;
    int idxRep = 1;
    if (i == 0) {
      idx = 1;
      idxRep = 0;
    }
    int curr = 0;
    for (int j = 0; j < N; ++j) {
      int dig = s[j] - '0';
      curr += dig * pat[idx];
      idxRep++;
      if (idxRep >= rep) {
        idx = (idx + 1) % PATSIZE;
        idxRep = 0;
      }
    }
    s[i] = ((abs(curr) % 10) + '0');
  }
  return fft(s, n - 1);
}

int main() {
  ifstream fin("day16.in");
  string str;
  fin >> str;
  fin.close();
  cout << "Part 1: " << fft(str, 100).substr(0, 8) << endl;
  cout << "Part 2: " << fft1(str, 100) << endl;
  return 0;
}
