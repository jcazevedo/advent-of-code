#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

vector<vector<int>> histories;

void input() {
  ifstream fin("input/day09.in");
  string s;
  while (getline(fin, s)) {
    vector<int> history;
    istringstream ss(s);
    int tmp;
    while (ss >> tmp) { history.push_back(tmp); }
    histories.push_back(history);
  }
  fin.close();
}

int solve(bool last) {
  int ans = 0;
  int H = histories.size();
  for (int i = 0; i < H; ++i) {
    vector<vector<int>> seqs;
    seqs.push_back(histories[i]);
    while (true) {
      bool all0 = true;
      vector<int> next;
      for (int j = 1; j < (int)seqs.back().size(); ++j) {
        int diff = seqs.back()[j] - seqs.back()[j - 1];
        if (diff != 0) { all0 = false; }
        next.push_back(diff);
      }
      seqs.push_back(next);
      if (all0) { break; }
    }
    int next = 0;
    for (int j = seqs.size() - 1; j >= 0; --j) {
      if (last) {
        next = seqs[j].back() + next;
      } else {
        next = seqs[j][0] - next;
      }
    }
    ans += next;
  }
  return ans;
}

int part1() { return solve(true); }

int part2() { return solve(false); }

int main() {
  input();
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
