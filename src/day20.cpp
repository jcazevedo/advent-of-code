#include <bits/stdc++.h>

using namespace std;

typedef pair<int, int> pii;
typedef pair<pii, int> state;

vector<string> grid;
pii start, finish;
map<pii, pii> jumps;
set<pii> outer;
int dirs[][2] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

int main() {
  ifstream fin("day20.in");
  string str;
  while (getline(fin, str))
    grid.push_back(str);
  int H = grid.size();
  map<string, vector<pii>> portals;
  for (int i = 0; i < H; ++i) {
    int W = grid[i].size();
    for (int j = 0; j < W; ++j) {
      char ch = grid[i][j];
      if (isalpha(ch)) {
        string id = "";
        id.append(1, ch);
        pii pos = {-1, -1};
        if (j + 1 < W && isalpha(grid[i][j + 1])) {
          id += grid[i][j + 1];
          if (j + 2 < W && grid[i][j + 2] == '.')
            pos = {i, j + 2};
          if (j - 1 >= 0 && grid[i][j - 1] == '.')
            pos = {i, j - 1};
        } else if (i + 1 < H && isalpha(grid[i + 1][j])) {
          id += grid[i + 1][j];
          if (i + 2 < H && grid[i + 2][j] == '.')
            pos = {i + 2, j};
          if (i - 1 >= 0 && grid[i - 1][j] == '.')
            pos = {i - 1, j};
        }
        if (pos.first != -1) {
          portals[id].push_back(pos);
          if (i == 0 || j == 0 || i == H - 2 || j == W - 2)
            outer.insert(pos);
        }
      }
    }
  }
  for (auto itr = portals.begin(); itr != portals.end(); ++itr) {
    vector<pii> points = itr->second;
    if (itr->first == "AA" || itr->first == "ZZ") {
      assert(points.size() == 1);
      if (itr->first == "AA")
        start = points[0];
      else
        finish = points[0];
    } else {
      assert(points.size() == 2);
      jumps[points[0]] = points[1];
      jumps[points[1]] = points[0];
    }
  }
  map<pii, int> dist;
  queue<pii> q;
  dist[start] = 0;
  q.push(start);
  while (!q.empty()) {
    pii curr = q.front(); q.pop();
    for (int i = 0; i < 4; ++i) {
      int ni = curr.first + dirs[i][0];
      int nj = curr.second + dirs[i][1];
      pii next = {ni, nj};
      if (ni >= 0 && ni < H && nj >= 0 && nj < grid[ni].size() && grid[ni][nj] == '.' && dist.find(next) == dist.end()) {
        dist[next] = dist[curr] + 1;
        q.push(next);
      }
    }
    if (jumps.find(curr) != jumps.end()) {
      pii next = jumps[curr];
      if (dist.find(next) == dist.end()) {
        dist[next] = dist[curr] + 1;
        q.push(next);
      }
    }
  }
  cout << "Part 1: " << dist[finish] << endl;

  map<state, int> dist2;
  queue<state> q2;
  state start2 = make_pair(start, 0);
  dist2[start2] = 0;
  q2.push(start2);
  state finish2 = make_pair(finish, 0);
  while (!q2.empty() && dist2.find(finish2) == dist2.end()) {
    state curr = q2.front(); q2.pop();
    int level = curr.second;
    for (int i = 0; i < 4; ++i) {
      int ni = curr.first.first + dirs[i][0];
      int nj = curr.first.second + dirs[i][1];
      state next = {{ni, nj}, level};
      if (ni >= 0 && ni < H && nj >= 0 && nj < grid[ni].size() && grid[ni][nj] == '.' && dist2.find(next) == dist2.end()) {
        dist2[next] = dist2[curr] + 1;
        q2.push(next);
      }
    }
    if (jumps.find(curr.first) != jumps.end()) {
      if (outer.find(curr.first) != outer.end()) {
        if (level > 0) {
          state next = {jumps[curr.first], level - 1};
          if (dist2.find(next) == dist2.end()) {
            dist2[next] = dist2[curr] + 1;
            q2.push(next);
          }
        }
      } else {
        state next = {jumps[curr.first], level + 1};
        if (dist2.find(next) == dist2.end()) {
          dist2[next] = dist2[curr] + 1;
          q2.push(next);
        }
      }
    }
  }
  cout << "Part 2: " << dist2[finish2] << endl;
  return 0;
}
