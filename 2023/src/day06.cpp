#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

vector<int> t, d;

void readInput() {
  ifstream fin("input/day06.in");
  string tmp, s;
  int v;
  getline(fin, tmp);
  istringstream sst(tmp);
  sst >> s;
  while (sst >> v) { t.push_back(v); }
  getline(fin, tmp);
  istringstream ssd(tmp);
  ssd >> s;
  while (ssd >> v) { d.push_back(v); }
  fin.close();
}

int part1() {
  int ans = 1, N = t.size();
  for (int i = 0; i < N; ++i) {
    int ways = 0;
    for (int tt = 0; tt < t[i]; ++tt) {
      int distance = tt * (t[i] - tt);
      if (distance > d[i]) { ++ways; }
    }
    ans *= ways;
  }
  return ans;
}

long long part2() {
  string st, sd;
  ostringstream osst, ossd;
  for (int tt : t) { osst << tt; }
  for (int dd : d) { ossd << dd; }
  st = osst.str();
  sd = ossd.str();
  int time;
  long long distance;
  istringstream isst(st), issd(sd);
  isst >> time;
  issd >> distance;
  long long ans = 0;
  for (int tt = 0; tt < time; ++tt) {
    long long newDistance = tt * (time - tt);
    if (newDistance > distance) { ++ans; }
  }
  return ans;
}

int main() {
  readInput();
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
