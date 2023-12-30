#include <fstream>
#include <iostream>
#include <tuple>
#include <unordered_map>
#include <vector>

using namespace std;

vector<tuple<int, int>> DIRS = {{1, 0}, {-1, 0}, {0, -1}, {0, 1}};
unordered_map<char, tuple<int, int>> NEXT_DIR = {
    {'v', {1, 0}}, {'^', {-1, 0}}, {'>', {0, 1}}, {'<', {0, -1}}};

vector<string> grid;
int H, W;

void input() {
  ifstream fin("input/day23.in");
  string tmp;
  while (fin >> tmp) { grid.push_back(tmp); }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

vector<vector<bool>> visited;

int dfs(int si, int sj, int ei, int ej, bool slopes = true, int dist = 0) {
  if (si == ei && sj == ej) { return dist; }
  int ans = 0;
  if (grid[si][sj] != '.' && slopes) {
    int ni = si + get<0>(NEXT_DIR[grid[si][sj]]);
    int nj = sj + get<1>(NEXT_DIR[grid[si][sj]]);
    if (ni >= 0 && ni < H && nj >= 0 && nj < W && !visited[ni][nj]) {
      visited[ni][nj] = true;
      ans = dfs(ni, nj, ei, ej, slopes, dist + 1);
      visited[ni][nj] = false;
    }
  } else {
    for (const tuple<int, int>& dir : DIRS) {
      int di = get<0>(dir);
      int dj = get<1>(dir);
      int ni = si + di;
      int nj = sj + dj;
      if (ni >= 0 && ni < H && nj >= 0 && nj < W && grid[ni][nj] != '#' &&
          !visited[ni][nj]) {
        visited[ni][nj] = true;
        ans = max(ans, dfs(ni, nj, ei, ej, slopes, dist + 1));
        visited[ni][nj] = false;
      }
    }
  }
  return ans;
}

int main() {
  input();
  int si = 0, sj = -1;
  for (int j = 0; j < W && sj == -1; ++j) {
    if (grid[si][j] == '.') { sj = j; }
  }
  int ei = H - 1, ej = -1;
  for (int j = 0; j < W && ej == -1; ++j) {
    if (grid[ei][j] == '.') { ej = j; }
  }
  visited = vector<vector<bool>>(H, vector<bool>(W, false));
  visited[si][sj] = true;
  int p1 = dfs(si, sj, ei, ej);
  cout << "Part 1: " << p1 << endl;
  int p2 = dfs(si, sj, ei, ej, false);
  cout << "Part 2: " << p2 << endl;
  return 0;
}
