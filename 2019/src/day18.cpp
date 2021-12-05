#include <bits/stdc++.h>

using namespace std;

typedef pair<int, int> pii;
typedef pair<int, pii> state;
typedef pair<int, vector<pii>> state1;
int dirs[][2] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

int getShortestPath(vector<string> grid) {
  int H = grid.size(), W = grid[0].size(), K = 'z' - 'a' + 1, T = (1 << K) - 1;
  pii startP = {-1, -1};
  for (int i = 0; i < H && startP.first == -1; ++i) {
    for (int j = 0; j < W && startP.first == -1; ++j) {
      if (grid[i][j] == '@') {
        grid[i][j] = '.';
        startP = {i, j};
      }
    }
  }
  map<state, int> dist;
  queue<state> q;
  state start = {0, startP};
  dist[start] = 0;
  q.push(start);
  while (!q.empty()) {
    state curr = q.front(); q.pop();
    int keys = curr.first;
    int i = curr.second.first;
    int j = curr.second.second;
    for (int d = 0; d < 4; ++d) {
      int ni = i + dirs[d][0];
      int nj = j + dirs[d][1];
      if (ni >= 0 && ni < H && nj >= 0 && nj < W && grid[ni][nj] != '#') {
        char p = grid[ni][nj];
        if (p == '.' || (isupper(p) && ((keys & (1 << (tolower(p) - 'a'))) > 0))) {
          state next = {keys, {ni, nj}};
          if (dist.find(next) == dist.end()) {
            dist[next] = dist[curr] + 1;
            q.push(next);
          }
        } else if (islower(p)) {
          int nextKeys = keys | (1 << (p - 'a'));
          if (nextKeys == T)
            return dist[curr] + 1;
          state next = {nextKeys, {ni, nj}};
          if (dist.find(next) == dist.end()) {
            dist[next] = dist[curr] + 1;
            q.push(next);
          }
        }
      }
    }
  }
  return -1;
}

int getShortestPathMultiple(vector<string> grid) {
  int H = grid.size(), W = grid[0].size(), K = 'z' - 'a' + 1, T = (1 << K) - 1;
  vector<pii> startP;
  vector<set<state>> visited;
  for (int i = 0; i < H && startP.size() == 0; ++i) {
    for (int j = 0; j < W && startP.size() == 0; ++j) {
      if (grid[i][j] == '@') {
        for (int ni = i - 1; ni <= i + 1; ++ni) {
          for (int nj = j - 1; nj <= j + 1; ++nj) {
            if (abs(ni - i) + abs(nj - j) == 2) {
              startP.push_back({ni, nj});
              visited.push_back(set<state>());
              visited.back().insert({0, {ni, nj}});
              grid[ni][nj] = '.';
            } else {
              grid[ni][nj] = '#';
            }
          }
        }
      }
    }
  }
  map<state1, int> dist;
  queue<state1> q;
  state1 start = {0, startP};
  dist[start] = 0;
  q.push(start);
  while (!q.empty()) {
    state1 curr = q.front(); q.pop();
    int keys = curr.first;
    vector<pii> pos = curr.second;
    for (int r = 0; r < 4; ++r) {
      int i = pos[r].first;
      int j = pos[r].second;
      for (int d = 0; d < 4; ++d) {
        int ni = i + dirs[d][0];
        int nj = j + dirs[d][1];
        if (ni >= 0 && ni < H && nj >= 0 && nj < W && grid[ni][nj] != '#') {
          char p = grid[ni][nj];
          vector<pii> nextPos = pos;
          nextPos[r] = {ni, nj};
          if (p == '.' || (isupper(p) && ((keys & (1 << (tolower(p) - 'a'))) > 0))) {
            state1 next = {keys, nextPos};
            state nextI = {keys, {ni, nj}};
            if (dist.find(next) == dist.end() && visited[r].find(nextI) == visited[r].end()) {
              dist[next] = dist[curr] + 1;
              visited[r].insert(nextI);
              q.push(next);
            }
          } else if (islower(p)) {
            int nextKeys = keys | (1 << (p - 'a'));
            if (nextKeys == T)
              return dist[curr] + 1;
            state1 next = {nextKeys, nextPos};
            state nextI = {nextKeys, {ni, nj}};
            if (dist.find(next) == dist.end() && visited[r].find(nextI) == visited[r].end()) {
              dist[next] = dist[curr] + 1;
              visited[r].insert(nextI);
              q.push(next);
            }
          }
        }
      }
    }
  }
  return -1;
}

int main() {
  ifstream fin("day18.in");
  string str;
  vector<string> grid;
  while (fin >> str)
    grid.push_back(str);
  cout << "Part 1: " << getShortestPath(grid) << endl;
  cout << "Part 2: " << getShortestPathMultiple(grid) << endl;
  return 0;
}
