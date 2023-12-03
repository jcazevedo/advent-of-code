#include <cctype>
#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

tuple<int, int> solve(const vector<string>& grid) {
  int M = grid.size(), N = grid[0].size(), part1 = 0, part2 = 0;
  vector<vector<bool>> visited(M, vector<bool>(N, false));
  for (int i = 0; i < M; ++i) {
    for (int j = 0; j < N; ++j) {
      if (isdigit(grid[i][j]) || grid[i][j] == '.') { continue; }
      int gear = 1, gearNumbers = 0;
      for (int di = -1; di <= 1; ++di) {
        if (i + di < 0 || i + di >= M) { continue; }
        for (int dj = -1; dj <= 1; ++dj) {
          if (j + dj < 0 || j + dj >= N) { continue; }
          int ni = i + di;
          int nj = j + dj;
          if (isdigit(grid[ni][nj]) && !visited[ni][nj]) {
            while (nj >= 0 && isdigit(grid[ni][nj])) { nj--; }
            int num = 0;
            while (++nj < N && isdigit(grid[ni][nj])) {
              visited[ni][nj] = true;
              num = num * 10 + grid[ni][nj] - '0';
            }
            part1 += num;
            if (grid[i][j] == '*') {
              gear *= num;
              gearNumbers++;
            }
          }
        }
      }
      if (gearNumbers == 2) { part2 += gear; }
    }
  }
  return {part1, part2};
}

int main() {
  vector<string> grid;
  ifstream fin("input/day03.in");
  string s;
  while (fin >> s) { grid.push_back(s); }
  fin.close();
  int part1, part2;
  tie(part1, part2) = solve(grid);
  cout << "Part 1: " << part1 << endl;
  cout << "Part 2: " << part2 << endl;
}
