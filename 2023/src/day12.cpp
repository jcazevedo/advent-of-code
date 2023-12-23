#include <bits/stdc++.h>

using namespace std;

struct Condition {
  string spring;
  vector<int> groups;
};

vector<Condition> conditions;

void input() {
  ifstream fin("input/day12.in");
  string s, g;
  while (fin >> s >> g) {
    Condition c;
    c.spring = s;
    int curr = 0;
    for (char ch : g) {
      if (ch == ',') {
        c.groups.push_back(curr);
        curr = 0;
      } else {
        curr = curr * 10 + (ch - '0');
      }
    }
    c.groups.push_back(curr);
    conditions.push_back(c);
  }
  fin.close();
}

long long possibleArrangementsAux(string& spring,
                                  const vector<int>& groups,
                                  int i,
                                  int currGroupSize,
                                  int currGroup,
                                  map<tuple<int, int, int>, long long>& cache) {
  if (cache.find({i, currGroupSize, currGroup}) != cache.end()) {
    return cache[{i, currGroupSize, currGroup}];
  }
  if (i == (int)spring.size()) {
    if (spring[i - 1] == '#') {
      cache[{i, currGroupSize, currGroup}] =
          (currGroup + 1) == (int)groups.size() &&
          currGroupSize == groups[currGroup];
    } else {
      cache[{i, currGroupSize, currGroup}] = (currGroup == (int)groups.size());
    }
  }
  if (spring[i] == '?') {
    long long ans = 0;
    if (currGroupSize == 0 || currGroupSize == groups[currGroup]) {
      spring[i] = '.';
      int nextGroup = currGroup;
      if (currGroupSize > 0) { ++nextGroup; }
      ans +=
          possibleArrangementsAux(spring, groups, i + 1, 0, nextGroup, cache);
    }
    if (currGroup < (int)groups.size() &&
        currGroupSize + 1 <= groups[currGroup]) {
      spring[i] = '#';
      ans += possibleArrangementsAux(spring, groups, i + 1, currGroupSize + 1,
                                     currGroup, cache);
    }
    spring[i] = '?';
    cache[{i, currGroupSize, currGroup}] = ans;
  }
  if (spring[i] == '.') {
    if (currGroupSize == 0) {
      cache[{i, currGroupSize, currGroup}] = possibleArrangementsAux(
          spring, groups, i + 1, currGroupSize, currGroup, cache);
    } else if (currGroupSize != groups[currGroup]) {
      cache[{i, currGroupSize, currGroup}] = 0;
    } else {
      cache[{i, currGroupSize, currGroup}] = possibleArrangementsAux(
          spring, groups, i + 1, 0, currGroup + 1, cache);
    }
  }
  if (spring[i] == '#') {
    if (currGroup >= (int)groups.size() ||
        currGroupSize + 1 > groups[currGroup]) {
      cache[{i, currGroupSize, currGroup}] = 0;
    } else {
      cache[{i, currGroupSize, currGroup}] = possibleArrangementsAux(
          spring, groups, i + 1, currGroupSize + 1, currGroup, cache);
    }
  }
  return cache[{i, currGroupSize, currGroup}];
}

long long possibleArrangements(Condition c) {
  map<tuple<int, int, int>, long long> cache;
  return possibleArrangementsAux(c.spring, c.groups, 0, 0, 0, cache);
}

long long part1() {
  long long ans = 0;
  for (Condition c : conditions) {
    long long arr = possibleArrangements(c);
    ans += arr;
  }
  return ans;
}

long long part2() {
  long long ans = 0;
  for (Condition c : conditions) {
    string baseSpring = c.spring;
    vector<int> baseGroup = c.groups;
    for (int i = 0; i < 4; ++i) {
      c.spring += "?";
      c.spring += baseSpring;
      for (int v : baseGroup) { c.groups.push_back(v); }
    }
    long long arr = possibleArrangements(c);
    ans += arr;
  }
  return ans;
}

int main() {
  input();
  long long p1 = part1();
  cout << "Part 1: " << p1 << endl;
  long long p2 = part2();
  cout << "Part 2: " << p2 << endl;
  return 0;
}
