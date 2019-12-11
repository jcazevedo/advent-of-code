#include <bits/stdc++.h>

using namespace std;

map<long long, long long> readInput(string file) {
  ifstream fin(file);
  string str; fin >> str;
  fin.close();
  replace(str.begin(), str.end(), ',', ' ');
  map<long long, long long> res;
  istringstream ss(str);
  long long v;
  int idx = 0;
  while (ss >> v)
    res[idx++] = v;
  return res;
}

vector<int> getModes(long long instruction, int nModes) {
  instruction /= 100;
  vector<int> modes;
  while (nModes--) {
    modes.push_back(instruction % 10);
    instruction /= 10;
  }
  return modes;
}

long long getValue(map<long long, long long>& program, long long parameter, int mode, long long relBase, bool output = false) {
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

tuple<bool, long long> run(map<long long, long long>& program, queue<long long>& input, long long& intP, long long& relBase) {
  vector<int> modes;
  bool running = true;
  long long p1, p2, p3;
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

int print(map<long long, long long> program, map<pair<int, int>, int>& canvas, int start) {
  vector<pair<int, int>> directions = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};
  set<pair<int, int>> visited;
  int currDir = 0;
  pair<int, int> coord = make_pair(0, 0);
  canvas[coord] = start;
  long long intP = 0, relBase = 0;
  queue<long long> input;
  while (true) {
    input.push(canvas[coord]);
    auto res1 = run(program, input, intP, relBase);
    if (!get<0>(res1))
      break;
    auto res2 = run(program, input, intP, relBase);
    visited.insert(coord);
    canvas[coord] = get<1>(res1);
    int d = get<1>(res2);
    if (d == 0)
      d = -1;
    currDir = (currDir + 4 + d) % 4;
    coord.first += directions[currDir].first;
    coord.second += directions[currDir].second;
  }
  return visited.size();
}

int main() {
  map<long long, long long> program = readInput("day11.in");
  map<pair<int, int>, int> canvas1, canvas2;
  cout << "Part 1: " << print(program, canvas1, 0) << endl;
  print(program, canvas2, 1);
  cout << "Part 2:" << endl;
  int minX = numeric_limits<int>::max(),
      minY = numeric_limits<int>::max(),
      maxX = numeric_limits<int>::min(),
      maxY = numeric_limits<int>::min();
  for (auto itr = canvas2.begin(); itr != canvas2.end(); itr++) {
    minX = min(minX, itr->first.first);
    minY = min(minY, itr->first.second);
    maxX = max(maxX, itr->first.first);
    maxY = max(maxY, itr->first.second);
  }
  for (int y = 0; y <= maxY - minY; ++y) {
    for (int x = 0; x <= maxX - minX; ++x) {
      pair<int, int> rcoord = make_pair(x + minY, y + minY);
      int color = canvas2[rcoord];
      cout << (color ? '#' : '.');
    }
    cout << endl;
  }
  return 0;
}
