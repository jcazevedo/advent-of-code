#include <bits/stdc++.h>

using namespace std;

unordered_map<string, tuple<int, int>> directions = {
    {"U", {-1, 0}}, {"D", {1, 0}}, {"L", {0, -1}}, {"R", {0, 1}}};

struct Instruction {
  string direction, color;
  int length;
};

vector<Instruction> instructions;

void input() {
  ifstream fin("input/day18.in");
  Instruction i;
  while (fin >> i.direction >> i.length >> i.color) {
    i.color = i.color.substr(1, i.color.size() - 2);
    instructions.push_back(i);
  }
  fin.close();
}

int part1() {
  set<tuple<int, int>> dug;
  int i = 0, j = 0;
  dug.insert({i, j});
  int minI = 0, maxI = 0, minJ = 0, maxJ = 0;
  for (const Instruction& instr : instructions) {
    int di, dj;
    tie(di, dj) = directions[instr.direction];
    for (int l = 0; l < instr.length; ++l) {
      i += di;
      j += dj;
      minI = min(minI, i);
      maxI = max(maxI, i);
      minJ = min(minJ, j);
      maxJ = max(maxJ, j);
      dug.insert({i, j});
    }
  }
  --minI;
  ++maxI;
  --minJ;
  ++maxJ;
  int ans = (maxI - minI + 1) * (maxJ - minJ + 1);
  set<tuple<int, int>> visited;
  visited.insert({minI, minJ});
  queue<tuple<int, int>> q;
  q.push({minI, minJ});
  --ans;
  while (!q.empty()) {
    int ci, cj;
    tie(ci, cj) = q.front();
    q.pop();
    for (auto itr = directions.begin(); itr != directions.end(); ++itr) {
      int ni = ci + get<0>(itr->second);
      int nj = cj + get<1>(itr->second);
      if (ni >= minI && ni <= maxI && nj >= minJ && nj <= maxJ &&
          dug.find({ni, nj}) == dug.end() &&
          visited.find({ni, nj}) == visited.end()) {
        visited.insert({ni, nj});
        q.push({ni, nj});
        --ans;
      }
    }
  }
  return ans;
}

long long part2() {
  vector<long long> xx;
  vector<long long> yy;
  xx.push_back(0L);
  yy.push_back(0L);
  long long ans = 0;
  for (const Instruction& instr : instructions) {
    long long distance = 0;
    for (int i = 1; i <= 5; ++i) {
      distance = distance * 16;
      if (isdigit(instr.color[i])) {
        distance += instr.color[i] - '0';
      } else {
        distance += (instr.color[i] - 'a') + 10;
      }
    }
    string dir = "";
    if (instr.color[6] == '0') { dir = "R"; }
    if (instr.color[6] == '1') { dir = "D"; }
    if (instr.color[6] == '2') { dir = "L"; }
    if (instr.color[6] == '3') { dir = "U"; }
    int dx, dy;
    tie(dx, dy) = directions[dir];
    xx.push_back(xx.back() + distance * dx);
    yy.push_back(yy.back() + distance * dy);
    ans += distance;
  }
  ans += 2;
  assert(xx.back() == 0L);
  assert(yy.back() == 0L);
  xx.pop_back();
  yy.pop_back();
  int N = xx.size();
  int j = N - 1;
  for (int i = 0; i < N; ++i) {
    ans += (xx[j] + xx[i]) * (yy[j] - yy[i]);
    j = i;
  }
  return ans / 2L;
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  long long p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
