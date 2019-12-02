#include <bits/stdc++.h>

using namespace std;

vector<int> readInput() {
  ifstream fin("day02.in");
  string str; fin >> str;
  fin.close();
  replace(str.begin(), str.end(), ',', ' ');
  vector<int> res;
  istringstream ss(str);
  int v;
  while (ss >> v)
    res.push_back(v);
  return res;
}

int part1(vector<int> data, int v1, int v2) {
  data[1] = v1;
  data[2] = v2;
  int N = data.size();
  for (int i = 0; i < N; i += 4) {
    assert(data[i] == 1 || data[i] == 2 || data[i] == 99);
    if (data[i] == 99)
      return data[0];
    int p1 = data[i + 1];
    int p2 = data[i + 2];
    int t = data[i + 3];
    if (data[i] == 1)
      data[t] = data[p1] + data[p2];
    else if (data[i] == 2)
      data[t] = data[p1] * data[p2];
  }
  return data[0];
}

int part2(vector<int> data) {
  int N = data.size();
  for (int v1 = 0; v1 < N; ++v1) {
    for (int v2 = 0; v2 < N; ++v2) {
      int res = part1(data, v1, v2);
      if (res == 19690720)
        return 100 * v1 + v2;
    }
  }
  return -1;
}

int main() {
  vector<int> data = readInput();
  cout << "Part 1: " << part1(data, 12, 2) << endl;
  cout << "Part 2: " << part2(data) << endl;
  return 0;
}
