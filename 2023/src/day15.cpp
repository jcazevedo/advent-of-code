#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<string> steps;

void input() {
  ifstream fin("input/day15.in");
  string s;
  fin >> s;
  string step = "";
  for (char ch : s) {
    if (ch == ',') {
      steps.push_back(step);
      step.clear();
    } else {
      step += ch;
    }
  }
  steps.push_back(step);
  fin.close();
}

int HASH(const string& s) {
  int ans = 0;
  for (char ch : s) {
    ans += ch;
    ans *= 17;
    ans %= 256;
  }
  return ans;
}

int part1() {
  int ans = 0;
  for (const string& step : steps) { ans += HASH(step); }
  return ans;
}

int part2() {
  vector<vector<string>> labels(256);
  vector<vector<int>> focalLengths(256);
  for (const string& step : steps) {
    if (step.back() == '-') {
      string label = step.substr(0, step.size() - 1);
      int box = HASH(label);
      auto itr = find(labels[box].begin(), labels[box].end(), label);
      if (itr != labels[box].end()) {
        labels[box].erase(itr);
        focalLengths[box].erase(focalLengths[box].begin() +
                                (itr - labels[box].begin()));
      }
    } else {
      string label = step.substr(0, step.size() - 2);
      int focalLength = step.back() - '0';
      int box = HASH(label);
      auto itr = find(labels[box].begin(), labels[box].end(), label);
      if (itr != labels[box].end()) {
        focalLengths[box][itr - labels[box].begin()] = focalLength;
      } else {
        labels[box].push_back(label);
        focalLengths[box].push_back(focalLength);
      }
    }
  }
  int ans = 0;
  for (int box = 0; box < 256; ++box) {
    for (int i = 0; i < (int)labels[box].size(); ++i) {
      ans += (box + 1) * ((i + 1) * focalLengths[box][i]);
    }
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
