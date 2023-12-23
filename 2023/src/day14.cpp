#include <bits/stdc++.h>

using namespace std;

vector<string> grid;
int H, W;

void input() {
  ifstream fin("input/day14.in");
  string line;
  while (fin >> line) { grid.push_back(line); }
  fin.close();
  H = grid.size();
  W = grid[0].size();
}

void tiltNorth() {
  for (int c = 0; c < W; ++c) {
    int s = 0, rocks = 0;
    for (int r = 0; r < H; ++r) {
      if (grid[r][c] == 'O') { rocks++; }
      if (grid[r][c] == '#') {
        for (int j = s; j < r; ++j) {
          if (rocks > 0) {
            grid[j][c] = 'O';
            rocks--;
          } else {
            grid[j][c] = '.';
          }
        }
        s = r + 1;
      }
    }
    if (rocks > 0) {
      for (int j = s; j < H; ++j) {
        if (rocks > 0) {
          grid[j][c] = 'O';
          rocks--;
        } else {
          grid[j][c] = '.';
        }
      }
    }
  }
}

void tiltWest() {
  for (int r = 0; r < H; ++r) {
    int s = 0, rocks = 0;
    for (int c = 0; c < W; ++c) {
      if (grid[r][c] == 'O') { rocks++; }
      if (grid[r][c] == '#') {
        for (int j = s; j < c; ++j) {
          if (rocks > 0) {
            grid[r][j] = 'O';
            rocks--;
          } else {
            grid[r][j] = '.';
          }
        }
        s = c + 1;
      }
    }
    if (rocks > 0) {
      for (int j = s; j < W; ++j) {
        if (rocks > 0) {
          grid[r][j] = 'O';
          rocks--;
        } else {
          grid[r][j] = '.';
        }
      }
    }
  }
}

void tiltSouth() {
  for (int c = 0; c < W; ++c) {
    int s = H - 1, rocks = 0;
    for (int r = H - 1; r >= 0; --r) {
      if (grid[r][c] == 'O') { rocks++; }
      if (grid[r][c] == '#') {
        for (int j = s; j > r; --j) {
          if (rocks > 0) {
            grid[j][c] = 'O';
            rocks--;
          } else {
            grid[j][c] = '.';
          }
        }
        s = r - 1;
      }
    }
    if (rocks > 0) {
      for (int j = s; j >= 0; --j) {
        if (rocks > 0) {
          grid[j][c] = 'O';
          rocks--;
        } else {
          grid[j][c] = '.';
        }
      }
    }
  }
}

void tiltEast() {
  for (int r = 0; r < H; ++r) {
    int s = W - 1, rocks = 0;
    for (int c = W - 1; c >= 0; --c) {
      if (grid[r][c] == 'O') { rocks++; }
      if (grid[r][c] == '#') {
        for (int j = s; j > c; --j) {
          if (rocks > 0) {
            grid[r][j] = 'O';
            rocks--;
          } else {
            grid[r][j] = '.';
          }
        }
        s = c - 1;
      }
    }
    if (rocks > 0) {
      for (int j = s; j >= 0; --j) {
        if (rocks > 0) {
          grid[r][j] = 'O';
          rocks--;
        } else {
          grid[r][j] = '.';
        }
      }
    }
  }
}

int load() {
  int ans = 0;
  for (int r = 0; r < H; ++r) {
    for (int c = 0; c < W; ++c) {
      if (grid[r][c] == 'O') { ans += H - r; }
    }
  }
  return ans;
}

int part1() {
  vector<string> prev = grid;
  tiltNorth();
  int ans = load();
  grid = prev;
  return ans;
}

void cycle() {
  tiltNorth();
  tiltWest();
  tiltSouth();
  tiltEast();
}

int part2() {
  vector<string> prev = grid;
  map<vector<string>, int> visited;
  visited[grid] = 0;
  int cycles = 0;
  int repeat = 0;
  while (true) {
    cycle();
    ++cycles;
    if (visited.find(grid) != visited.end()) {
      repeat = visited[grid];
      break;
    }
    visited[grid] = cycles;
  }
  int totCycles = 1000000000 - cycles;
  int cnt = totCycles % (cycles - repeat);
  for (int i = 0; i < cnt; ++i) { cycle(); }
  int ans = load();
  grid = prev;
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
