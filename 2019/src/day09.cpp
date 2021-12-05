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

int main() {
  map<long long, long long> program = readInput("day09.in");
  queue<long long> input1;
  input1.push(1);
  vector<long long> output1 = runContinuously(program, input1);
  assert(output1.size() > 0);
  cout << "Part 1: " << output1.back() << endl;
  queue<long long> input2;
  input2.push(2);
  vector<long long> output2 = runContinuously(program, input2);
  assert(output2.size() > 0);
  cout << "Part 2: " << output2.back() << endl;
  return 0;
}
