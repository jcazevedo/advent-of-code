#include <bits/stdc++.h>

using namespace std;

int N;
vector<long double> px, py, pz, vx, vy, vz;

void input() {
  ifstream fin("input/day24.in");
  string line;
  vector<string> lines;
  while (getline(fin, line)) { lines.push_back(line); }
  fin.close();
  N = lines.size();
  px = vector<long double>(N);
  py = vector<long double>(N);
  pz = vector<long double>(N);
  vx = vector<long double>(N);
  vy = vector<long double>(N);
  vz = vector<long double>(N);
  for (int i = 0; i < N; ++i) {
    sscanf(lines[i].c_str(), "%Lf, %Lf, %Lf @ %Lf, %Lf, %Lf", &px[i], &py[i],
           &pz[i], &vx[i], &vy[i], &vz[i]);
  }
}

tuple<long double, long double> infP = {
    numeric_limits<long double>::infinity(),
    numeric_limits<long double>::infinity()};

tuple<long double, long double> intersection(int sa, int sb) {
  if (vx[sa] / vx[sb] == vy[sa] / vy[sb]) { return infP; }
  long double a = (py[sb] - py[sa] + (px[sa] - px[sb]) / vx[sb] * vy[sb]) /
                  (vy[sa] - vx[sa] * vy[sb] / vx[sb]);
  if (a < 0) { return infP; }
  long double b = (px[sa] - px[sb] + a * vx[sa]) / vx[sb];
  if (b < 0) { return infP; }
  return {px[sa] + a * vx[sa], py[sa] + a * vy[sa]};
}

int part1() {
  long double minV = 200000000000000.0, maxV = 400000000000000.0;
  int ans = 0;
  numeric_limits<double>::infinity();
  for (int i = 0; i < N; ++i) {
    for (int j = i + 1; j < N; ++j) {
      tuple<long double, long double> intrP = intersection(i, j);
      if (intrP == infP) { continue; }
      long double x = get<0>(intrP);
      long double y = get<1>(intrP);
      if (x < minV || x > maxV) { continue; }
      if (y < minV || y > maxV) { continue; }
      ++ans;
    }
  }
  return ans;
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  // Part 2 involved following the following system of equations:
  //
  // px + t1 * vx = 225004689740965 + t1 * 275
  // px + t2 * vx = 338282582546422 + t2 * -162
  // px + t3 * vx = 276063330011297 + t3 * -9
  // py + t1 * vy = 150875733412640 + t1 * 389
  // py + t2 * vy = 191340608518886 + t2 * 84
  // py + t3 * vy = 506267063607948 + t3 * -360
  // pz + t1 * vz = 116049940893518 + t1 * 375
  // pz + t2 * vz = 340003210160681 + t2 * -46
  // pz + t3 * vz = 451688278442130 + t3 * -275
  return 0;
}
