#include <bits/stdc++.h>

using namespace std;

int sgn(int d) {
  if (d < 0)
    return -1;
  if (d > 0)
    return 1;
  return 0;
}

long long gcd(long long a, long long b) {
  if (b == 0)
    return a;
  return gcd(b, a % b);
}

long long lcm(long long a, long long b) {
  return (a * b) / gcd(a, b);
}

int main() {
  FILE* fp = fopen("day12.in", "r");
  vector<int> xx, yy, zz, vx, vy, vz;
  int x, y, z;
  while (fscanf(fp, "<x=%d, y=%d, z=%d>\n", &x, &y, &z) != EOF) {
    xx.push_back(x);
    yy.push_back(y);
    zz.push_back(z);
    vx.push_back(0);
    vy.push_back(0);
    vz.push_back(0);
  }
  int N = xx.size(), tot = 0, t = 0;
  vector<int> sx = xx, sy = yy, sz = zz, svx = vx, svy = vy, svz = vz;
  vector<int> rep = {-1, -1, -1};
  while (rep[0] == -1 || rep[1] == -1 || rep[2] == -1) {
    if (t == 1000) {
      for (int i = 0; i < N; ++i) {
        int pot = 0;
        int kin = 0;
        pot += abs(xx[i]);
        pot += abs(yy[i]);
        pot += abs(zz[i]);
        kin += abs(vx[i]);
        kin += abs(vy[i]);
        kin += abs(vz[i]);
        tot += pot * kin;
      }
    }
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        vx[i] += sgn(xx[j] - xx[i]);
        vy[i] += sgn(yy[j] - yy[i]);
        vz[i] += sgn(zz[j] - zz[i]);
      }
    }
    for (int i = 0; i < N; ++i) {
      xx[i] += vx[i];
      yy[i] += vy[i];
      zz[i] += vz[i];
    }
    t++;
    if (rep[0] == -1 && xx == sx && vx == svx)
      rep[0] = t;
    if (rep[1] == -1 && yy == sy && vy == svy)
      rep[1] = t;
    if (rep[2] == -1 && zz == sz && vz == svz)
      rep[2] = t;
  }
  cout << "Part 1: " << tot << endl;
  cout << "Part 2: " << lcm(lcm(rep[0], rep[1]), rep[2]) << endl;
  return 0;
}
