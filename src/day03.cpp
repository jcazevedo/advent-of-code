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

int closestIntersection(const vector<vector<Instruction> >& wires) {
  map<pair<int, int>, int> visited;
  map<char, pair<int, int> > directions =
      {{'U', make_pair(0, 1)}, {'D', make_pair(0, -1)}, {'L', make_pair(-1, 0)}, {'R', make_pair(1, 0)}};
  int nWires = wires.size();
  int best = numeric_limits<int>::max();
  visited[make_pair(0, 0)] = 1;
  for (int w = 1; w <= nWires; ++w) {
    vector<Instruction> steps = wires[w - 1];
    int N = steps.size(), x = 0, y = 0;
    for (int i = 0; i < N; ++i) {
      pair<int, int> dirs = directions[steps[i].direction];
      int nSteps = steps[i].nSteps;
      int dx = dirs.first;
      int dy = dirs.second;
      for (int j = 0; j < nSteps; ++j) {
        int nx = x + dx;
        int ny = y + dy;
        pair<int, int> next = make_pair(nx, ny);
        if (visited[next] != 0 && visited[next] != w)
          best = min(best, abs(nx) + abs(ny));
        visited[next] = w;
        x = nx;
        y = ny;
      }
    }
  }
  return best;
}

int main() {
  vector<vector<Instruction> > wires = readInput();
  cout << "Part 1: " << closestIntersection(wires) << endl;
  return 0;
}
