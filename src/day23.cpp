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

tuple<bool, ll> tick(map<ll, ll>& program, queue<ll>& input, ll& intP, ll& relBase) {
  vector<int> modes;
  bool running = true;
  ll p1, p2, p3;
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
      if (input.empty()) {
        program[p1] = -1;
      } else {
        program[p1] = input.front();
        input.pop();
      }
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
  return {running, -1};
}

ll runAll(map<ll, ll> program, int computers) {
  vector<map<ll, ll>> programs;
  vector<queue<ll>> inputs;
  vector<ll> intPs;
  vector<ll> relBases;
  vector<vector<ll>> outputs;
  vector<bool> done;
  for (int i = 0; i < computers; ++i) {
    programs.push_back(program);
    queue<ll> input;
    input.push(i);
    inputs.push_back(input);
    intPs.push_back(0);
    relBases.push_back(0);
    outputs.push_back(vector<ll>());
    done.push_back(false);
  }
  int doneCnt = 0;
  int idx = 0;
  while (doneCnt < computers) {
    if (!done[idx]) {
      tuple<bool, ll> res = tick(programs[idx], inputs[idx], intPs[idx], relBases[idx]);
      ll output = get<1>(res);
      done[idx] = !get<0>(res);
      if (done[idx])
        doneCnt++;
      if (output != -1)
        outputs[idx].push_back(output);
      if (outputs[idx].size() == 3) {
        ll target = outputs[idx][0];
        if (target == 255)
          return outputs[idx][2];
        inputs[target].push(outputs[idx][1]);
        inputs[target].push(outputs[idx][2]);
        outputs[idx].clear();
      }
    }
    idx = (idx + 1) % computers;
  }
  return -1;
}

int main() {
  map<ll, ll> program = readInput("day23.in");
  cout << "Part 1: " << runAll(program, 50) << endl;
  return 0;
}
