#include <cassert>
#include <fstream>
#include <iostream>
#include <queue>
#include <set>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

vector<tuple<int, int>> DIRS = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

vector<string> grid;
int H, W;

void input() {
  ifstream fin("input/day21.in");
  string line;
  while (fin >> line) { grid.push_back(line); }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

int nReachable(int si, int sj, int steps) {
  set<tuple<int, int>> ans;
  set<tuple<int, int>> visited;
  queue<tuple<int, int, int>> q;
  visited.insert({si, sj});
  q.push({si, sj, steps});
  while (!q.empty()) {
    int i, j, s;
    tie(i, j, s) = q.front();
    q.pop();
    if (s % 2 == 0) { ans.insert({i, j}); };
    if (s == 0) { continue; }
    for (const tuple<int, int>& dir : DIRS) {
      int ni = i + get<0>(dir);
      int nj = j + get<1>(dir);
      if (ni >= 0 && ni < H && nj >= 0 && nj < W && grid[ni][nj] != '#' &&
          visited.find({ni, nj}) == visited.end()) {
        visited.insert({ni, nj});
        q.push({ni, nj, s - 1});
      }
    }
  }
  return ans.size();
}

int part1() {
  int si = -1, sj = -1;
  for (int i = 0; i < H && si == -1 && sj == -1; ++i) {
    for (int j = 0; j < W && si == -1 && sj == -1; ++j) {
      if (grid[i][j] == 'S') {
        si = i;
        sj = j;
      }
    }
  }
  return nReachable(si, sj, 64);
}

long long part2() {
  long long cnt = 26501365L;
  int si = -1, sj = -1;
  for (int i = 0; i < H && si == -1 && sj == -1; ++i) {
    for (int j = 0; j < W && si == -1 && sj == -1; ++j) {
      if (grid[i][j] == 'S') {
        si = i;
        sj = j;
      }
    }
  }
  assert(H == W);
  assert(si == H / 2 && sj == W / 2);
  assert(cnt % H == H / 2);
  long long gridWidth = cnt / H - 1;
  long long oddGrids = (gridWidth / 2 * 2 + 1) * (gridWidth / 2 * 2 + 1);
  long long evenGrids = ((gridWidth + 1) / 2 * 2) * ((gridWidth + 1) / 2 * 2);
  long long reachableOdds = nReachable(si, sj, H * 2 + 1);
  long long reachableEven = nReachable(si, sj, H * 2);
  long long reachableTop = nReachable(H - 1, sj, H - 1);
  long long reachableRight = nReachable(si, 0, W - 1);
  long long reachableLeft = nReachable(si, W - 1, W - 1);
  long long reachableBottom = nReachable(0, sj, H - 1);
  long long reachableTriangleTR = nReachable(H - 1, 0, H / 2 - 1);
  long long reachableTriangleBR = nReachable(0, 0, H / 2 - 1);
  long long reachableTriangleTL = nReachable(H - 1, W - 1, H / 2 - 1);
  long long reachableTriangleBL = nReachable(0, W - 1, H / 2 - 1);
  long long reachableTrapezoidTR = nReachable(H - 1, 0, H * 3 / 2 - 1);
  long long reachableTrapezoidBR = nReachable(0, 0, H * 3 / 2 - 1);
  long long reachableTrapezoidTL = nReachable(H - 1, W - 1, H * 3 / 2 - 1);
  long long reachableTrapezoidBL = nReachable(0, W - 1, H * 3 / 2 - 1);
  long long ans =
      oddGrids * reachableOdds + evenGrids * reachableEven + reachableTop +
      reachableRight + reachableLeft + reachableBottom +
      (gridWidth + 1) * (reachableTriangleTR + reachableTriangleBR +
                         reachableTriangleTL + reachableTriangleBL) +
      gridWidth * (reachableTrapezoidTR + reachableTrapezoidBR +
                   reachableTrapezoidTL + reachableTrapezoidBL);
  return ans;
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  long long p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
