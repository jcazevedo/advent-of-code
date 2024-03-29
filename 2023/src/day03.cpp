#include <cctype>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
  vector<string> grid;
  ifstream fin("input/day03.in");
  string s;
  while (fin >> s) { grid.push_back(s); }
  fin.close();
  int M = grid.size(), N = grid[0].size(), part1 = 0, part2 = 0;
  for (int i = 0; i < M; ++i) {
    for (int j = 0; j < N; ++j) {
      if (isdigit(grid[i][j]) || grid[i][j] == '.') { continue; }
      int gear = 1, gearNumbers = 0;
      for (int di = -1; di <= 1; ++di) {
        int ni = i + di;
        if (ni < 0 || ni >= M) { continue; }
        for (int dj = -1; dj <= 1; ++dj) {
          int nj = j + dj;
          if (nj < 0 || nj >= N || !isdigit(grid[ni][nj])) { continue; }
          while (nj >= 0 && isdigit(grid[ni][nj])) { nj--; }
          int num = 0;
          while (++nj < N && isdigit(grid[ni][nj])) {
            num = num * 10 + grid[ni][nj] - '0';
            grid[ni][nj] = '.';
          }
          part1 += num;
          if (grid[i][j] == '*') {
            gear *= num;
            gearNumbers++;
          }
        }
      }
      if (gearNumbers == 2) { part2 += gear; }
    }
  }
  cout << "Part 1: " << part1 << endl;
  cout << "Part 2: " << part2 << endl;
}
