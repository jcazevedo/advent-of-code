#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <tuple>
#include <vector>

using namespace std;

vector<string> grid;
map<tuple<int, int>, vector<char>> connections = {{{-1, 0}, {'|', '7', 'F'}},
                                                  {{1, 0}, {'|', 'L', 'J'}},
                                                  {{0, -1}, {'-', 'L', 'F'}},
                                                  {{0, 1}, {'-', 'J', '7'}}};

void input() {
  ifstream fin("input/day10.in");
  string tmp;
  while (fin >> tmp) { grid.push_back(tmp); }
  fin.close();
}

tuple<int, int> solve() {
  int H = grid.size(), W = grid[0].size(), si = -1, sj = -1;
  for (int i = 0; i < H && si == -1; ++i) {
    for (int j = 0; j < W && si == -1; ++j) {
      if (grid[i][j] == 'S') {
        si = i;
        sj = j;
      }
    }
  }
  int maxDist = 0;
  set<tuple<int, int>> inLoop;
  queue<tuple<int, int, int>> q1;
  q1.push({si, sj, 0});
  inLoop.insert({si, sj});
  while (!q1.empty()) {
    int i, j, d;
    tie(i, j, d) = q1.front();
    q1.pop();
    maxDist = max(maxDist, d);
    for (auto itr = connections.begin(); itr != connections.end(); ++itr) {
      int ni = i + get<0>(itr->first);
      int nj = j + get<1>(itr->first);
      const vector<char>& expected =
          connections[{-get<0>(itr->first), -get<1>(itr->first)}];
      if (ni >= 0 && ni < H && nj >= 0 && nj < W &&
          find(itr->second.begin(), itr->second.end(), grid[ni][nj]) !=
              itr->second.end() &&
          (find(expected.begin(), expected.end(), grid[i][j]) !=
               expected.end() ||
           grid[i][j] == 'S') &&
          inLoop.find({ni, nj}) == inLoop.end()) {
        q1.push({ni, nj, d + 1});
        inLoop.insert({ni, nj});
      }
    }
  }
  vector<tuple<int, int>> sConns;
  for (auto itr = connections.begin(); itr != connections.end(); ++itr) {
    int ni = si + get<0>(itr->first);
    int nj = sj + get<1>(itr->first);
    if (inLoop.find({ni, nj}) != inLoop.end() &&
        find(itr->second.begin(), itr->second.end(), grid[ni][nj]) !=
            itr->second.end()) {
      sConns.push_back(itr->first);
    }
  }
  if (find(sConns.begin(), sConns.end(), make_tuple(-1, 0)) != sConns.end()) {
    if (find(sConns.begin(), sConns.end(), make_tuple(1, 0)) != sConns.end()) {
      grid[si][sj] = '|';
    } else if (find(sConns.begin(), sConns.end(), make_tuple(0, -1)) !=
               sConns.end()) {
      grid[si][sj] = 'J';
    } else if (find(sConns.begin(), sConns.end(), make_tuple(0, 1)) !=
               sConns.end()) {
      grid[si][sj] = 'L';
    }
  } else if (find(sConns.begin(), sConns.end(), make_tuple(1, 0)) !=
             sConns.end()) {
    if (find(sConns.begin(), sConns.end(), make_tuple(0, -1)) != sConns.end()) {
      grid[si][sj] = '7';
    } else if (find(sConns.begin(), sConns.end(), make_tuple(0, 1)) !=
               sConns.end()) {
      grid[si][sj] = 'F';
    }
  } else {
    grid[si][sj] = '-';
  }
  int insideLoop = 0;
  for (int i = 0; i < H; ++i) {
    bool inside = false;
    char prev = '.';
    for (int j = 0; j < W; ++j) {
      if (inLoop.find({i, j}) != inLoop.end()) {
        if (grid[i][j] == '|' || (grid[i][j] == 'J' && prev == 'F') ||
            (grid[i][j] == '7' && prev == 'L')) {
          inside = !inside;
        }
        if (grid[i][j] == 'J' || grid[i][j] == 'F' || grid[i][j] == '7' ||
            grid[i][j] == 'L') {
          prev = grid[i][j];
        }
      } else if (inside) {
        ++insideLoop;
      }
    }
  }
  return {maxDist, insideLoop};
}

int main() {
  input();
  int part1, part2;
  tie(part1, part2) = solve();
  cout << "Part 1: " << part1 << endl;
  cout << "Part 2: " << part2 << endl;
  return 0;
}
