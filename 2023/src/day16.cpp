#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

vector<tuple<int, int>> DIRS = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
map<tuple<int, int, char>, vector<tuple<int, int>>> NEXT_DIR = {
    {{0, 1, '.'}, {{0, 1}}},           {{1, 0, '.'}, {{1, 0}}},
    {{0, -1, '.'}, {{0, -1}}},         {{-1, 0, '.'}, {{-1, 0}}},
    {{0, 1, '\\'}, {{1, 0}}},          {{1, 0, '\\'}, {{0, 1}}},
    {{0, -1, '\\'}, {{-1, 0}}},        {{-1, 0, '\\'}, {{0, -1}}},
    {{0, 1, '/'}, {{-1, 0}}},          {{1, 0, '/'}, {{0, -1}}},
    {{0, -1, '/'}, {{1, 0}}},          {{-1, 0, '/'}, {{0, 1}}},
    {{0, 1, '-'}, {{0, 1}}},           {{1, 0, '-'}, {{0, -1}, {0, 1}}},
    {{0, -1, '-'}, {{0, -1}}},         {{-1, 0, '-'}, {{0, -1}, {0, 1}}},
    {{0, 1, '|'}, {{-1, 0}, {1, 0}}},  {{1, 0, '|'}, {{1, 0}}},
    {{0, -1, '|'}, {{-1, 0}, {1, 0}}}, {{-1, 0, '|'}, {{-1, 0}}},
};

vector<string> grid;
int H, W;

void input() {
  ifstream fin("input/day16.in");
  string line;
  while (fin >> line) { grid.push_back(line); }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

int energy(int si, int sj, tuple<int, int> sdir) {
  vector<vector<bool>> energized(H, vector<bool>(W, false));
  set<tuple<int, int, tuple<int, int>>> visited;
  vector<tuple<int, int, tuple<int, int>>> current;
  current.push_back({si, sj, sdir});
  visited.insert({si, sj, sdir});
  energized[si][sj] = true;
  while (!current.empty()) {
    vector<tuple<int, int, tuple<int, int>>> next;
    for (const tuple<int, int, tuple<int, int>>& pos : current) {
      int i = get<0>(pos);
      int j = get<1>(pos);
      int di, dj;
      tie(di, dj) = get<2>(pos);
      for (const tuple<int, int>& ndir : NEXT_DIR[{di, dj, grid[i][j]}]) {
        int ni = i + get<0>(ndir);
        int nj = j + get<1>(ndir);
        if (ni >= 0 && ni < H && nj >= 0 && nj < W &&
            visited.find({ni, nj, ndir}) == visited.end()) {
          next.push_back({ni, nj, ndir});
          visited.insert({ni, nj, ndir});
          energized[ni][nj] = true;
        }
      }
    }
    current = next;
  }
  int ans = 0;
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (energized[i][j]) { ++ans; }
    }
  }
  return ans;
}

int part1() { return energy(0, 0, {0, 1}); }

int part2() {
  int ans = 0;
  for (int i = 0; i < W; ++i) {
    ans = max(ans, energy(0, i, {1, 0}));
    ans = max(ans, energy(H - 1, i, {-1, 0}));
  }
  for (int i = 0; i < H; ++i) {
    ans = max(ans, energy(i, 0, {0, 1}));
    ans = max(ans, energy(i, W - 1, {0, -1}));
  }
  return ans;
}

int main() {
  input();
  int p1 = part1();
  cout << "Part 1: " << p1 << endl;
  int p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
