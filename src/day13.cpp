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

vector<long long> runContinuously(map<long long, long long> program, queue<long long> input) {
  vector<long long> output;
  long long intP = 0, relBase = 0;
  while (true) {
    auto res = run(program, input, intP, relBase);
    if (!get<0>(res))
      break;
    output.push_back(get<1>(res));
  }
  return output;
}

void fill(const vector<long long>& output, map<pair<long long, long long>, int>& tiles, int& score) {
  int N = output.size();
  assert(N % 3 == 0);
  for (int i = 0; i < N; i += 3) {
    long long x = output[i];
    long long y = output[i + 1];
    int tileId = output[i + 2];
    if (x == -1 && y == 0)
      score = tileId;
    else
      tiles[{x, y}] = tileId;
  }
}

int countBlockTiles(const vector<long long>& output) {
  map<pair<long long, long long>, int> tiles;
  int score = 0;
  fill(output, tiles, score);
  int cnt = 0;
  for (auto itr = tiles.begin(); itr != tiles.end(); ++itr) {
    if (itr->second == 2)
      cnt++;
  }
  return cnt;
}

int sgn(int d) {
  if (d < 0)
    return -1;
  if (d > 0)
    return 1;
  return 0;
}

int getScore(map<long long, long long> program) {
  program[0] = 2;
  long long intP = 0, relBase = 0;
  queue<long long> input;
  map<pair<long long, long long>, int> tiles;
  int score = 0;
  bool ball = false, paddle = false;
  int bx, px, cnt = 0;
  while (true) {
    long long x = get<1>(run(program, input, intP, relBase));
    long long y = get<1>(run(program, input, intP, relBase));
    long long id = get<1>(run(program, input, intP, relBase));
    if (x == -1 && y == -1 && id == -1)
      break;
    if (x == -1 && y == 0) {
      score = id;
    } else {
      if (id == 2 && tiles[{x, y}] != 2)
        cnt++;
      if (id != 2 && tiles[{x, y}] == 2)
        cnt--;
      tiles[{x, y}] = id;
    }
    if (id == 4) {
      bx = x;
      ball = true;
    }
    if (id == 3) {
      px = x;
      paddle = true;
    }
    if (ball && paddle) {
      input.push(sgn(bx - px));
      ball = false;
    }
  }
  return score;
}

int main() {
  map<long long, long long> program = readInput("day13.in");
  queue<long long> input;
  vector<long long> output = runContinuously(program, input);
  cout << "Part 1: " << countBlockTiles(output) << endl;
  cout << "Part 2: " << getScore(program) << endl;
  return 0;
}
