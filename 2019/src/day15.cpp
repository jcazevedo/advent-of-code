#include <bits/stdc++.h>

using namespace std;

typedef long long ll;
typedef pair<int, int> pii;
const map<int, pii> dirs = {{1, {0, -1}}, {2, {0, 1}}, {3, {-1, 0}}, {4, {1, 0}}};

map<ll, ll> readInput(string file) {
  ifstream fin(file);
  string str; fin >> str;
  fin.close();
  replace(str.begin(), str.end(), ',', ' ');
  map<ll, ll> res;
  istringstream ss(str);
  ll v;
  int idx = 0;
  while (ss >> v)
    res[idx++] = v;
  return res;
}

vector<int> getModes(ll instruction, int nModes) {
  instruction /= 100;
  vector<int> modes;
  while (nModes--) {
    modes.push_back(instruction % 10);
    instruction /= 10;
  }
  return modes;
}

ll getValue(map<ll, ll>& program, ll parameter, int mode, ll relBase, bool output = false) {
  assert(mode == 0 || mode == 1 || mode == 2);
  if (mode == 0 && !output)
    return program[parameter];
  if (mode == 0 && output)
    return parameter;
  if (mode == 2 && !output)
    return program[parameter + relBase];
  if (mode == 2 && output)
    return parameter + relBase;
  return parameter;
}

bool validInstruction(int instruction) {
  static vector<int> validCodes = {1, 2, 3, 4, 5, 6, 7, 8, 9, 99};
  for (int code : validCodes)
    if (code == instruction)
      return true;
  return false;
}

tuple<bool, ll> run(map<ll, ll>& program, queue<ll>& input, ll& intP, ll& relBase) {
  vector<int> modes;
  bool running = true;
  ll p1, p2, p3;
  while (running) {
    vector<int> modes;
    int instruction = program[intP] % 100;
    assert(validInstruction(instruction));
    switch (instruction) {
      case 1:
        modes = getModes(program[intP], 3);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        p3 = getValue(program, program[intP + 3], modes[2], relBase, true);
        program[p3] = p1 + p2;
        intP += 4;
        break;

      case 2:
        modes = getModes(program[intP], 3);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        p3 = getValue(program, program[intP + 3], modes[2], relBase, true);
        program[p3] = p1 * p2;
        intP += 4;
        break;

      case 3:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0], relBase, true);
        program[p1] = input.front(); input.pop();
        intP += 2;
        break;

      case 4:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        intP += 2;
        return {running, p1};

      case 5:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        if (p1 != 0)
          intP = p2;
        else
          intP += 3;
        break;

      case 6:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        if (p1 == 0)
          intP = p2;
        else
          intP += 3;
        break;

      case 7:
        modes = getModes(program[intP], 3);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        p3 = getValue(program, program[intP + 3], modes[2], relBase, true);
        program[p3] = p1 < p2;
        intP += 4;
        break;

      case 8:
        modes = getModes(program[intP], 3);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        p2 = getValue(program, program[intP + 2], modes[1], relBase);
        p3 = getValue(program, program[intP + 3], modes[2], relBase, true);
        program[p3] = p1 == p2;
        intP += 4;
        break;

      case 9:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        relBase += p1;
        intP += 2;
        break;

      case 99:
        running = false;
        break;
    }
  }
  return {running, -1};
}

pii diff(pii curr, int dir) {
  return {curr.first + dirs.at(dir).first, curr.second + dirs.at(dir).second};
}

void bfs(const map<pii, int>& grid, pii from, map<pii, int>& dist, map<pii, int>& prev) {
  dist.clear();
  prev.clear();
  queue<pii> q;
  dist[from] = 0;
  prev[from] = -1;
  q.push(from);
  while (!q.empty()) {
    pii curr = q.front(); q.pop();
    for (int i = 1; i <= 4; ++i) {
      pii next = diff(curr, i);
      if (dist.find(next) == dist.end()) {
        if (grid.find(next) != grid.end() && grid.at(next) != 0) {
          dist[next] = dist[curr] + 1;
          prev[next] = i;
          q.push(next);
        }
        if (dist.find(next) == dist.end()) {
          dist[next] = dist[curr] + 1;
          prev[next] = i;
        }
      }
    }
  }
}

int getNext(const map<pii, int>& grid, const map<pii, int>& dist, const map<pii, int>& prev) {
  int minDist = numeric_limits<int>::max();
  pii minPos = {-1, -1};
  for (auto itr = dist.begin(); itr != dist.end(); itr++) {
    pii pos = itr->first;
    int dist = itr->second;
    if (grid.find(pos) == grid.end() && dist < minDist) {
      minDist = dist;
      minPos = pos;
    }
  }
  if (minDist == numeric_limits<int>::max())
    return -1;
  int prevD = -1;
  while (prev.at(minPos) != -1) {
    prevD = prev.at(minPos);
    minPos = {minPos.first - dirs.at(prevD).first, minPos.second - dirs.at(prevD).second};
  }
  return prevD;
}

int main() {
  map<ll, ll> program = readInput("day15.in");
  map<pii, int> grid, dist, prev;
  pii curr = {0, 0};
  pii ox = {0, 0};
  grid[curr] = 0;
  queue<ll> input;
  ll intP = 0, relBase = 0;
  while (true) {
    bfs(grid, curr, dist, prev);
    int in = getNext(grid, dist, prev);
    if (in == -1)
      break;
    pii next = diff(curr, in);
    input.push(in);
    auto res = run(program, input, intP, relBase);
    int id = get<1>(res);
    switch (id) {
      case 0:
        grid[next] = 0;
        break;
      case 1:
        grid[next] = 1;
        curr = next;
        break;
      case 2:
        grid[next] = 2;
        ox = next;
        curr = next;
        break;
    }
  }
  bfs(grid, {0, 0}, dist, prev);
  cout << "Part 1: " << dist[ox] << endl;
  bfs(grid, ox, dist, prev);
  int maxDist = numeric_limits<int>::min();
  for (auto itr = dist.begin(); itr != dist.end(); itr++) {
    maxDist = max(maxDist, itr->second);
  }
  cout << "Part 2: " << maxDist << endl;
  return 0;
}
