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

tuple<bool, int> run(map<int, int>& program, queue<int>& input, int& intP) {
  vector<int> modes, params;
  bool running = true;
  int p1, p2, p3;
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
        program[p1] = input.front(); input.pop();
        intP += 2;
        break;

      case 4:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0]);
        intP += 2;
        return make_tuple(running, p1);

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
  return make_tuple(running, -1);
}

vector<int> runContinuously(map<int, int> program, queue<int> input) {
  vector<int> output;
  int intP = 0;
  while (true) {
    auto res = run(program, input, intP);
    if (!get<0>(res))
      break;
    output.push_back(get<1>(res));
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
      queue<int> in;
      in.push(i);
      in.push(input);
      vector<int> out = runContinuously(program, in);
      assert(out.size() > 0);
      best = max(best, getBestOutputAux(program, thrusters - 1, out.back(), used));
      used.erase(i);
    }
  }
  return best;
}

int getBestOutput(const map<int, int>& program) {
  set<int> used;
  return getBestOutputAux(program, 5, 0, used);
}

int getBestOutputLoopAux(const map<int, int>& program, int thrusters, vector<int>& signals, set<int>& used) {
  if (thrusters == 0) {
    vector<map<int, int> > programs = vector<map<int, int> >(5, program);
    vector<queue<int> > inputs = vector<queue<int> >(5, queue<int>());
    vector<int> inputP = vector<int>(5, 0);
    for (size_t i = 0; i < signals.size(); ++i)
      inputs[i].push(signals[i]);
    inputs[0].push(0);
    int currentP = 0;
    int output = 0;
    while (true) {
      auto res = run(programs[currentP], inputs[currentP], inputP[currentP]);
      if (!get<0>(res)) {
        return output;
      }
      currentP = (currentP + 1) % 5;
      output = get<1>(res);
      inputs[currentP].push(output);
    }
  }
  int best = 0;
  for (int i = 5; i <= 9; ++i) {
    if (used.find(i) == used.end()) {
      used.insert(i);
      signals.push_back(i);
      best = max(best, getBestOutputLoopAux(program, thrusters - 1, signals, used));
      signals.pop_back();
      used.erase(i);
    }
  }
  return best;
}

int getBestOutputLoop(const map<int, int>& program) {
  set<int> used;
  vector<int> signals;
  return getBestOutputLoopAux(program, 5, signals, used);
}

int main() {
  map<int, int> program = readInput("day07.in");
  cout << "Part 1: " << getBestOutput(program) << endl;
  cout << "Part 2: " << getBestOutputLoop(program) << endl;
  return 0;
}
