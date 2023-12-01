#include <cassert>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

struct TrieNode {
  unordered_map<char, TrieNode*> next;
  int value;

  TrieNode() : value(-1) {}
};

void add(TrieNode* node, const string& key, int value, int idx = 0) {
  if (idx == (int)key.size()) {
    node->value = value;
  } else {
    if (!node->next.count(key[idx])) { node->next[key[idx]] = new TrieNode(); }
    add(node->next[key[idx]], key, value, idx + 1);
  }
}

int get(TrieNode* node, const string& key, int idx = 0) {
  if (idx == (int)key.size()) { return node->value; }
  if (!node->next.count(key[idx])) { return -1; }
  return get(node, key, idx + 1);
}

int find(TrieNode* node, const string& line, int idx) {
  if (node->value != -1) { return node->value; }
  if (idx == (int)line.size()) { return -1; }
  if (!node->next.count(line[idx])) { return -1; }
  return find(node->next[line[idx]], line, idx + 1);
}

int solve(TrieNode* trie, const vector<string>& lines) {
  int ans = 0;
  for (const string& line : lines) {
    int left = -1, right = -1;
    for (int i = 0; i < (int)line.size(); ++i) {
      int curr = find(trie, line, i);
      if (curr != -1) {
        left = curr;
        break;
      }
    }
    for (int i = (int)line.size() - 1; i >= 0; --i) {
      int curr = find(trie, line, i);
      if (curr != -1) {
        right = curr;
        break;
      }
    }
    assert(left != -1 && right != -1);
    ans += left * 10 + right;
  }
  return ans;
}

int main() {
  vector<string> lines;
  ifstream fin("input/day01.in");
  string s;
  while (fin >> s) { lines.push_back(s); }
  fin.close();
  TrieNode* trie = new TrieNode();
  for (char ch = '0'; ch <= '9'; ++ch) {
    string key = "";
    key += ch;
    add(trie, key, ch - '0');
  }
  int part1 = solve(trie, lines);
  add(trie, "zero", 0);
  add(trie, "one", 1);
  add(trie, "two", 2);
  add(trie, "three", 3);
  add(trie, "four", 4);
  add(trie, "five", 5);
  add(trie, "six", 6);
  add(trie, "seven", 7);
  add(trie, "eight", 8);
  add(trie, "nine", 9);
  int part2 = solve(trie, lines);
  cout << "Part 1: " << part1 << endl;
  cout << "Part 2: " << part2 << endl;
  return 0;
}
