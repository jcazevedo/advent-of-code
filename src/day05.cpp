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
  static vector<int> validCodes = {1, 2, 3, 4, 99};
  for (int code : validCodes)
    if (code == instruction)
      return true;
  return false;
}

vector<int> run(map<int, int> program, const vector<int>& input) {
  vector<int> output, modes;
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

      case 99:
        running = false;
        break;
    }
  }
  return output;
}

int main() {
  map<int, int> program = readInput("day05.in");
  vector<int> input = {1};
  vector<int> output = run(program, input);
  cout << "Part 1: " << output.back() << endl;
  return 0;
}
