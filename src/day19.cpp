#include <bits/stdc++.h>

using namespace std;

typedef long long ll;

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

bool hasPull(map<ll, ll> program, int i, int j) {
  queue<ll> input;
  ll intP = 0, relBase = 0;
  input.push(i);
  input.push(j);
  return get<1>(run(program, input, intP, relBase)) == 1;
}

int getPoints(map<ll, ll> program) {
  int cnt = 0;
  for (int i = 0; i < 50; ++i) {
    for (int j = 0; j < 50; ++j) {
      if (hasPull(program, i, j))
        cnt++;
    }
  }
  return cnt;
}

#define SQUARE_SIDE 100
#define MAX_SIDE 10000

int getMinSquare(map<ll, ll> program) {
  static int grid[MAX_SIDE][MAX_SIDE], dp[MAX_SIDE][MAX_SIDE];
  memset(grid, 0, sizeof(grid));
  memset(dp, 0, sizeof(dp));
  for (int i = 0; i < MAX_SIDE; ++i) {
    for (int j = 0; j < MAX_SIDE; ++j) {
      int u = i > 0 ? dp[i - 1][j] : 0;
      int l = j > 0 ? dp[i][j - 1] : 0;
      int ul = (i > 0 && j > 0) ? dp[i - 1][j - 1] : 0;
      dp[i][j] = u + l - ul;
      if (i + j > 20) {
        int ng = 0;
        ng += i > 0 ? grid[i - 1][j] : 0;
        ng += j > 0 ? grid[i][j - 1] : 0;
        if (ng > 0 && (ng == 2 || hasPull(program, i, j))) {
          grid[i][j] = 1;
          dp[i][j]++;
        }
      } else {
        if (hasPull(program, i, j)) {
          grid[i][j] = 1;
          dp[i][j]++;
        }
      }
    }
  }
  int bestX = -1, bestY = -1;
  for (int i = 0; i + SQUARE_SIDE <= MAX_SIDE; ++i) {
    for (int j = 0; j + SQUARE_SIDE <= MAX_SIDE; ++j) {
      int ti = i + SQUARE_SIDE - 1;
      int tj = j + SQUARE_SIDE - 1;
      int sum = dp[ti][tj];
      sum -= (i > 0 ? dp[i - 1][tj] : 0);
      sum -= (j > 0 ? dp[ti][j - 1] : 0);
      sum += (i > 0 && j > 0 ? dp[i - 1][j - 1] : 0);
      if (sum == SQUARE_SIDE * SQUARE_SIDE && (bestY == -1 || i + j < bestX + bestY)) {
        bestX = i;
        bestY = j;
      }
    }
  }
  return bestX * 10000 + bestY;
}

int main() {
  map<ll, ll> program = readInput("day19.in");
  cout << "Part 1: " << getPoints(program) << endl;
  cout << "Part 2: " << getMinSquare(program) << endl;
  return 0;
}
