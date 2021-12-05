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

void stringToInput(queue<ll>& input, string str) {
  for (char ch : str)
    input.push(ch);
}

void run(map<ll, ll>& program) {
  queue<ll> input;
  vector<int> modes;
  bool running = true;
  ll p1, p2, p3, intP = 0, relBase = 0;
  while (running) {
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
          string str;
          getline(cin, str);
          stringToInput(input, str + "\n");
        }
        program[p1] = input.front(); input.pop();
        intP += 2;
        break;

      case 4:
        modes = getModes(program[intP], 1);
        p1 = getValue(program, program[intP + 1], modes[0], relBase);
        intP += 2;
        cout << (char)(p1);
        break;

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
}

int main() {
  map<ll, ll> program = readInput("day25.in");
  run(program);
  return 0;
}
