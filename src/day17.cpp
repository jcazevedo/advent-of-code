#include <bits/stdc++.h>

using namespace std;

typedef long long ll;
typedef pair<int, int> pii;
typedef pair<pii, pii> seg;

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

vector<string> buildGrid(map<ll, ll> program) {
  vector<string> res;
  string current = "";
  queue<ll> input;
  ll intP = 0, relBase = 0;
  while (true) {
    auto output = run(program, input, intP, relBase);
    if (!get<0>(output))
      break;
    char in = get<1>(output);
    if (in == '\n') {
      res.push_back(current);
      current = "";
    } else {
      current += in;
    }
  }
  return res;
}

map<char, int> dirs = {{'^', 0}, {'>', 1}, {'v', 2}, {'<', 3}};
int dd[][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

int alignments(const vector<string>& data) {
  int tot = 0;
  int H = data.size();
  for (int i = 1; i < H - 1; ++i) {
    int W = data[i].size();
    for (int j = 1; j < W - 1; ++j) {
      if (data[i][j] == '#') {
        bool inters = true;
        for (int k = 0; k < 4; ++k) {
          if (data[i + dd[k][0]][j + dd[k][1]] != '#') {
            inters = false;
            break;
          }
        }
        if (inters)
          tot += (i * j);
      }
    }
  }
  return tot;
}

pii next(pii curr, int dir) {
  return {curr.first + dd[dir][0], curr.second + dd[dir][1]};
}

char pos(const vector<string>& grid, pii curr) {
  if (curr.first < 0 || curr.first >= grid.size() || curr.second < 0 || curr.second >= grid[curr.first].size())
    return ' ';
  return grid[curr.first][curr.second];
}

string addInstruction(string prev, string next) {
  if (prev.size() != 0)
    prev += ",";
  prev += next;
  return prev;
}

string replaceAll(string original, string toReplace, string replacement) {
  int idx = 0;
  for (idx = original.find(toReplace, idx); idx != string::npos; idx = original.find(toReplace))
    original.replace(idx, toReplace.size(), replacement);
  return original;
}

string compress(string instructions, map<string, string>& programs, int curr = 0, int idx = 0) {
  int N = instructions.size();
  if (curr == 3) {
    if (instructions.size() > 20)
      return "";
    set<char> used;
    for (int i = 0; i < N; ++i) {
      if (instructions[i] == ',')
        continue;
      if (instructions[i] >= 'A' && instructions[i] <= 'C')
        used.insert(instructions[i]);
      else
        return "";
    }
    if (used.size() != 3)
      return "";
    return instructions;
  }
  for (int j = idx + 1; j < N; ++j) {
    if (j + 1 < N && instructions[j + 1] != ',')
      continue;
    if (instructions[j] >= 'A' && instructions[j] <= 'C')
      break;
    string target = instructions.substr(idx, j - idx + 1);
    if (target.size() > 20)
      break;
    char id = 'A' + curr;
    string pid = "";
    pid.append(1, id);
    programs[pid] = target;
    string nextInstructions = replaceAll(instructions, target, pid);
    int nextIdx = 0;
    int NN = nextInstructions.size();
    while (nextIdx < NN && ((nextInstructions[nextIdx] >= 'A' && nextInstructions[nextIdx] <= 'C') || nextInstructions[nextIdx] == ','))
      nextIdx++;
    string attempt = compress(nextInstructions, programs, curr + 1, nextIdx);
    if (attempt != "")
      return attempt;
    programs.erase(attempt);
  }
  return "";
}

string getInstructionsAux(
    const vector<string>& grid,
    map<string, string>& programs,
    pii currPos,
    int currDir,
    set<pii> scaf,
    set<seg>& visited,
    string instructions) {
  if (scaf.size() == 0) {
    string res = compress(instructions, programs);
    if (res != "") {
      return res;
    }
  } else {
    pii nextPos = next(currPos, currDir);
    if (pos(grid, nextPos) != '#') {
      int d = (currDir + 1) % 4;
      pii n = next(currPos, d);
      if (pos(grid, n) == '#') {
        string res = getInstructionsAux(grid, programs, currPos, d, scaf, visited, addInstruction(instructions, "R"));
        if (res != "")
          return res;
      }
      d = (currDir + 4 - 1) % 4;
      n = next(currPos, d);
      if (pos(grid, n) == '#') {
        string res = getInstructionsAux(grid, programs, currPos, d, scaf, visited, addInstruction(instructions, "L"));
        if (res != "")
          return res;
      }
    } else {
      pii prevPos = currPos;
      int cnt = 0;
      while (pos(grid, nextPos) == '#') {
        cnt++;
        currPos = nextPos;
        scaf.erase(currPos);
        nextPos = next(currPos, currDir);
      }
      seg currSeg = make_pair(min(currPos, prevPos), max(currPos, prevPos));
      if (visited.find(currSeg) == visited.end()) {
        visited.insert(currSeg);
        ostringstream ss;
        ss << cnt;
        string res = getInstructionsAux(grid, programs, currPos, currDir, scaf, visited, addInstruction(instructions, ss.str()));
        if (res != "")
          return res;
        visited.erase(currSeg);
      }
    }
  }
  return "";
}

string getInstructions(const vector<string>& grid, map<string, string>& programs) {
  pii currPos = {-1, -1};
  int currDir = -1, H = grid.size();
  set<pii> scaf;
  for (int i = 0; i < H; ++i) {
    int W = grid[i].size();
    for (int j = 0; j < W; ++j) {
      if (grid[i][j] != '#' && grid[i][j] != '.') {
        currPos = {i, j};
        currDir = dirs[grid[i][j]];
      }
      if (grid[i][j] == '#')
        scaf.insert({i, j});
    }
  }
  set<seg> visited;
  return getInstructionsAux(grid, programs, currPos, currDir, scaf, visited, "");
}

ll getDust(const vector<string>& grid, map<ll, ll> program) {
  program[0] = 2;
  map<string, string> programs;
  string instructions = getInstructions(grid, programs);
  queue<ll> input;
  ll intP = 0, relBase = 0;
  for (char inp : instructions)
    input.push(inp);
  input.push('\n');
  for (char p = 'A'; p <= 'C'; ++p) {
    string pid = "";
    pid.append(1, p);
    string program = programs[pid];
    for (char inp : program) {
      input.push(inp);
    }
    input.push('\n');
  }
  input.push('n');
  input.push('\n');
  ll res = -1;
  while (true) {
    auto output = run(program, input, intP, relBase);
    if (!get<0>(output))
      break;
    res = get<1>(output);
  }
  return res;
}

int main() {
  map<ll, ll> program = readInput("day17.in");
  vector<string> data = buildGrid(program);
  cout << "Part 1: " << alignments(data) << endl;
  cout << "Part 2: " << getDust(data, program) << endl;
  return 0;
}
