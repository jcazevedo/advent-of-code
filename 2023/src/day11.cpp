#include <bits/stdc++.h>

using namespace std;

vector<tuple<int, int>> DIRECTIONS = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

vector<string> grid;
int H, W;
vector<vector<bool>> isEmpty;
map<tuple<int, int>, map<tuple<int, int>, long long>> dist;

void input() {
  ifstream fin("input/day11.in");
  string tmp;
  while (fin >> tmp) { grid.push_back(tmp); }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

void setUpEmpties() {
  isEmpty = vector<vector<bool>>(H, vector<bool>(W, false));
  for (int i = 0; i < H; ++i) {
    bool empty = true;
    for (int j = 0; j < W && empty; ++j) {
      if (grid[i][j] == '#') { empty = false; }
    }
    if (empty) {
      for (int j = 0; j < W; ++j) { isEmpty[i][j] = true; }
    }
  }
  for (int j = 0; j < W; ++j) {
    bool empty = true;
    for (int i = 0; i < H && empty; ++i) {
      if (grid[i][j] == '#') { empty = false; }
    }
    if (empty) {
      for (int i = 0; i < H; ++i) { isEmpty[i][j] = true; }
    }
  }
}

void computeDistances(long long emptyCost) {
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (grid[i][j] != '#') { continue; }
      map<tuple<int, int>, long long> currentDistances;
      priority_queue<tuple<long long, int, int>,
                     vector<tuple<long long, int, int>>,
                     greater<tuple<long long, int, int>>>
          pq;
      currentDistances[{i, j}] = 0L;
      pq.push({0L, i, j});
      while (!pq.empty()) {
        long long d;
        int ci, cj;
        tie(d, ci, cj) = pq.top();
        pq.pop();
        if (grid[ci][cj] == '#') { dist[{i, j}][{ci, cj}] = d; }
        for (const tuple<int, int>& dir : DIRECTIONS) {
          int ni = ci + get<0>(dir);
          int nj = cj + get<1>(dir);
          if (ni >= 0 && ni < H && nj >= 0 && nj < W) {
            long long cost = 1L;
            if (isEmpty[ni][nj]) { cost = emptyCost; }
            if (currentDistances.find({ni, nj}) == currentDistances.end() ||
                currentDistances[{ni, nj}] > d + cost) {
              pq.push({d + cost, ni, nj});
              currentDistances[{ni, nj}] = d + cost;
            }
          }
        }
      }
    }
  }
}

long long lengthSum() {
  long long ans = 0;
  set<tuple<int, int>> visited;
  for (int i = 0; i < H; ++i) {
    for (int j = 0; j < W; ++j) {
      if (grid[i][j] != '#') { continue; }
      for (int k = 0; k < H; ++k) {
        for (int l = 0; l < W; ++l) {
          if (grid[k][l] != '#' || visited.find({k, l}) != visited.end()) {
            continue;
          }
          ans += dist[{i, j}][{k, l}];
        }
      }
      visited.insert({i, j});
    }
  }
  return ans;
}

int main() {
  input();
  setUpEmpties();
  computeDistances(2L);
  cout << "Part 1: " << lengthSum() << endl;
  computeDistances(1000000L);
  cout << "Part 2: " << lengthSum() << endl;
  return 0;
}
