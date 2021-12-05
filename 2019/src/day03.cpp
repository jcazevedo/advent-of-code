#include <bits/stdc++.h>

using namespace std;

struct Instruction {
  char direction;
  int nSteps;
};

vector<vector<Instruction> > readInput() {
  ifstream fin("day03.in");
  string str;
  vector<vector<Instruction> > res;
  while (fin >> str) {
    replace(str.begin(), str.end(), ',', ' ');
    istringstream ss(str);
    string v;
    vector<Instruction> curr;
    while (ss >> v) {
      Instruction u;
      sscanf(v.c_str(), "%c%d", &u.direction, &u.nSteps);
      curr.push_back(u);
    }
    res.push_back(curr);
  }
  fin.close();
  return res;
}

pair<int, int> closestIntersection(const vector<vector<Instruction> >& wires) {
  map<pair<int, int>, int> visited;
  map<pair<int, int>, int> dist;
  map<char, pair<int, int> > directions =
      {{'U', make_pair(0, 1)}, {'D', make_pair(0, -1)}, {'L', make_pair(-1, 0)}, {'R', make_pair(1, 0)}};
  int nWires = wires.size();
  int bestDist = numeric_limits<int>::max();
  int bestSteps = numeric_limits<int>::max();
  visited[make_pair(0, 0)] = 1;
  for (int w = 1; w <= nWires; ++w) {
    vector<Instruction> steps = wires[w - 1];
    int N = steps.size(), x = 0, y = 0, currSteps = 0;
    for (int i = 0; i < N; ++i) {
      pair<int, int> dirs = directions[steps[i].direction];
      int nSteps = steps[i].nSteps;
      int dx = dirs.first;
      int dy = dirs.second;
      for (int j = 0; j < nSteps; ++j) {
        int nx = x + dx;
        int ny = y + dy;
        currSteps++;
        pair<int, int> next = make_pair(nx, ny);
        if (visited[next] != w)
          dist[next] += currSteps;
        if (visited[next] != 0 && visited[next] != w) {
          bestDist = min(bestDist, abs(nx) + abs(ny));
          if (w == nWires)
            bestSteps = min(bestSteps, dist[next]);
        }
        visited[next] = w;
        x = nx;
        y = ny;
      }
    }
  }
  return make_pair(bestDist, bestSteps);
}

int main() {
  vector<vector<Instruction> > wires = readInput();
  pair<int, int> res = closestIntersection(wires);
  cout << "Part 1: " << res.first << endl;
  cout << "Part 2: " << res.second << endl;
  return 0;
}
