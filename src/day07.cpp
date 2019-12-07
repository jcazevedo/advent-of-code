#include <bits/stdc++.h>

using namespace std;

map<int, int> readInput(string file) {
  ifstream fin(file);
  string str; fin >> str;
  fin.close();
  replace(str.begin(), str.end(), ',', ' ');
  map<int, int> res;
  istringstream ss(str);
  int v, idx = 0;
  while (ss >> v)
    res[idx++] = v;
  return res;
}

vector<int> getModes(int instruction, int nModes) {
  instruction /= 100;
  vector<int> modes;
  while (nModes--) {
    modes.push_back(instruction % 10);
    instruction /= 10;
  }
  return modes;
}

int getValue(const map<int, int>& program, int parameter, int mode) {
  assert(mode == 0 || mode == 1);
  if (mode == 0)
    return program.at(parameter);
  return parameter;
}

bool validInstruction(int instruction) {
  static vector<int> validCodes = {1, 2, 3, 4, 5, 6, 7, 8, 99};
  for (int code : validCodes)
    if (code == instruction)
      return true;
  return false;
}

vector<int> run(map<int, int> program, const vector<int>& input) {
  vector<int> output, modes, params;
  bool running = true;
  int intP = 0, inputP = 0, p1, p2, p3;
  while (running) {
    vector<int> modes;
    int instruction = program[intP] % 100;
    assert(validInstruction(instruction));
    switch (instruction) {
      case 1:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        p3 = program[intP + 3];
        program[p3] = p1 + p2;
        intP += 4;
        break;

      case 2:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        p3 = program[intP + 3];
        program[p3] = p1 * p2;
        intP += 4;
        break;

      case 3:
        p1 = program[intP + 1];
        program[p1] = input[inputP++];
        intP += 2;
        break;

      case 4:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0]);
        output.push_back(p1);
        intP += 2;
        break;

      case 5:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        if (p1 != 0)
          intP = p2;
        else
          intP += 3;
        break;

      case 6:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        if (p1 == 0)
          intP = p2;
        else
          intP += 3;
        break;

      case 7:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        p3 = program[intP + 3];
        program[p3] = p1 < p2;
        intP += 4;
        break;

      case 8:
        modes = getModes(program[intP], 2);
        p1 = getValue(program, program[intP + 1], modes[0]);
        p2 = getValue(program, program[intP + 2], modes[1]);
        p3 = program[intP + 3];
        program[p3] = p1 == p2;
        intP += 4;
        break;

      case 99:
        running = false;
        break;
    }
  }
  return output;
}

int getBestOutputAux(const map<int, int>& program, int thrusters, int input, set<int>& used) {
  if (thrusters == 0)
    return input;
  int best = 0;
  for (int i = 0; i <= 4; ++i) {
    if (used.find(i) == used.end()) {
      used.insert(i);
      vector<int> in = {i, input};
      vector<int> out = run(program, in);
      assert(out.size() > 0);
      best = max(best, getBestOutputAux(program, thrusters - 1, out.back(), used));
      used.erase(i);
    }
  }
  return best;
}

int getBestOutput(const map<int, int>& program, int thrusters) {
  set<int> used = set<int>();
  return getBestOutputAux(program, thrusters, 0, used);
}

int main() {
  map<int, int> program = readInput("day07.in");
  cout << "Part 1: " << getBestOutput(program, 5) << endl;
  return 0;
}
