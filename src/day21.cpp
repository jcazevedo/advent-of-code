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

vector<ll> runContinuously(map<ll, ll> program, queue<ll> input) {
  vector<ll> output;
  ll intP = 0, relBase = 0;
  while (true) {
    auto res = run(program, input, intP, relBase);
    if (!get<0>(res))
      break;
    output.push_back(get<1>(res));
  }
  return output;
}

queue<ll> springCodeProgramToInput(const vector<string>& program) {
  queue<ll> res;
  int P = program.size();
  for (int i = 0; i < P; ++i) {
    int N = program[i].size();
    for (int j = 0; j < N; ++j)
      res.push(program[i][j]);
    res.push('\n');
  }
  return res;
}

void printOutput(const vector<ll>& output) {
  for (ll ch : output)
    cout << (char)(ch);
}

ll getHullDamage(const vector<ll>& output) {
  for (ll ch : output) {
    if (ch >= 256)
      return ch;
  }
  return -1;
}

int main() {
  map<ll, ll> program = readInput("day21.in");
  vector<string> springcodeProgram = {
      "NOT C J",
      "AND D J",
      "NOT A T",
      "OR T J",
      "WALK"};
  vector<ll> out1 = runContinuously(program, springCodeProgramToInput(springcodeProgram));
  cout << "Part 1: " << getHullDamage(out1) << endl;
  vector<string> extendedSpringcodeProgram = {
      "NOT A J",
      "NOT B T",
      "OR J T",
      "NOT C J",
      "OR J T",
      "AND D T",
      "AND H T",
      "NOT T J",
      "NOT J J",
      "NOT A T",
      "OR T J",
      "RUN"
  };
  vector<ll> out2 = runContinuously(program, springCodeProgramToInput(extendedSpringcodeProgram));
  cout << "Part 2: " << getHullDamage(out2) << endl;
  return 0;
}
