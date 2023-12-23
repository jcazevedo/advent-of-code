#include <bits/stdc++.h>

using namespace std;

vector<vector<string>> patterns;

void input() {
  ifstream fin("input/day13.in");
  string tmp;
  vector<string> curr;
  while (getline(fin, tmp)) {
    if (tmp == "") {
      patterns.push_back(curr);
      curr.clear();
    } else {
      curr.push_back(tmp);
    }
  }
  patterns.push_back(curr);
  fin.close();
}

set<int> notes(const vector<string>& grid) {
  set<int> ans;
  int H = grid.size(), W = grid[0].size();
  for (int c = 0; c < (W - 1); ++c) {
    bool good = true;
    for (int r = 0; r < H && good; ++r) {
      for (int i = 0; c - i >= 0 && c + 1 + i < W; ++i) {
        if (grid[r][c - i] != grid[r][c + 1 + i]) {
          good = false;
          break;
        }
      }
    }
    if (good) { ans.insert(c + 1); }
  }
  for (int r = 0; r < (H - 1); ++r) {
    bool good = true;
    for (int c = 0; c < W && good; ++c) {
      for (int i = 0; r - i >= 0 && r + 1 + i < H; ++i) {
        if (grid[r - i][c] != grid[r + 1 + i][c]) {
          good = false;
          break;
        }
      }
    }
    if (good) { ans.insert(100 * (r + 1)); }
  }
  return ans;
}

int part1() {
  int ans = 0;
  for (const vector<string>& pattern : patterns) {
    ans += *notes(pattern).begin();
  }
  return ans;
}

int part2() {
  int ans = 0;
  for (vector<string>& pattern : patterns) {
    set<int> original = notes(pattern);
    int H = pattern.size(), W = pattern[0].size();
    bool found = false;
    for (int r = 0; r < H && !found; ++r) {
      for (int c = 0; c < W && !found; ++c) {
        char prev = pattern[r][c];
        if (pattern[r][c] == '.') {
          pattern[r][c] = '#';
        } else {
          pattern[r][c] = '.';
        }
        set<int> newNotes = notes(pattern);
        for (int n : newNotes) {
          if (original.find(n) == original.end()) {
            ans += n;
            found = true;
            break;
          }
        }
        pattern[r][c] = prev;
      }
    }
    assert(found);
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
