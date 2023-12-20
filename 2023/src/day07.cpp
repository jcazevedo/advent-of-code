#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

#define FIVE_OF_A_KIND 1
#define FOUR_OF_A_KIND 2
#define FULL_HOUSE 3
#define THREE_OF_A_KIND 4
#define TWO_PAIR 5
#define ONE_PAIR 6
#define HIGH_CARD 7

vector<char> ALL_CARDS = {'2', '3', '4', '5', '6', '7', '8',
                          '9', 'T', 'J', 'Q', 'K', 'A'};
vector<char> CARD_ORDER_1 = {'2', '3', '4', '5', '6', '7', '8',
                             '9', 'T', 'J', 'Q', 'K', 'A'};
vector<char> CARD_ORDER_2 = {'J', '2', '3', '4', '5', '6', '7',
                             '8', '9', 'T', 'Q', 'K', 'A'};

struct Hand {
  string cards;
  int bid;
};

vector<Hand> hands;

void input() {
  ifstream fin("input/day07.in");
  Hand h;
  while (fin >> h.cards >> h.bid) { hands.push_back(h); }
  fin.close();
}

int type1(const string& cards) {
  unordered_map<char, int> count;
  for (char ch : cards) { count[ch]++; }
  unordered_map<int, vector<char>> reverseCount;
  for (char ch : ALL_CARDS) {
    if (count[ch] != 0) { reverseCount[count[ch]].push_back(ch); }
  }
  if (!reverseCount[5].empty()) { return FIVE_OF_A_KIND; }
  if (!reverseCount[4].empty()) { return FOUR_OF_A_KIND; }
  if (!reverseCount[3].empty() && !reverseCount[2].empty()) {
    return FULL_HOUSE;
  }
  if (!reverseCount[3].empty()) { return THREE_OF_A_KIND; }
  if (reverseCount[2].size() == 2) { return TWO_PAIR; }
  if (reverseCount[2].size() == 1) { return ONE_PAIR; }
  return HIGH_CARD;
}

int type2(const string& cards) {
  unordered_map<char, int> count;
  for (char ch : cards) { count[ch]++; }
  if (count['J'] == 5) { return FIVE_OF_A_KIND; }
  for (char ch : ALL_CARDS) {
    if (ch == 'J') { continue; }
    if (count[ch] + count['J'] >= 5) { return FIVE_OF_A_KIND; }
  }
  for (char ch : ALL_CARDS) {
    if (ch == 'J') { continue; }
    if (count[ch] + count['J'] >= 4) { return FOUR_OF_A_KIND; }
  }
  for (char ch1 : ALL_CARDS) {
    if (ch1 == 'J') { continue; }
    int rem = count['J'];
    if (count[ch1] + rem >= 3) {
      if (count[ch1] < 3) { rem -= 3 - count[ch1]; }
      for (char ch2 : ALL_CARDS) {
        if (ch2 == 'J' || ch2 == ch1) { continue; }
        if (count[ch2] + rem >= 2) { return FULL_HOUSE; }
      }
    }
  }
  for (char ch : ALL_CARDS) {
    if (ch == 'J') { continue; }
    if (count[ch] + count['J'] >= 3) { return THREE_OF_A_KIND; }
  }
  for (char ch1 : ALL_CARDS) {
    if (ch1 == 'J') { continue; }
    int rem = count['J'];
    if (count[ch1] + rem >= 2) {
      if (count[ch1] < 2) { rem -= 2 - count[ch1]; }
      for (char ch2 : ALL_CARDS) {
        if (ch2 == 'J' || ch2 == ch1) { continue; }
        if (count[ch2] + rem >= 2) { return TWO_PAIR; }
      }
    }
  }
  for (char ch : ALL_CARDS) {
    if (ch == 'J') { continue; }
    if (count[ch] + count['J'] >= 2) { return ONE_PAIR; }
  }
  return HIGH_CARD;
}

int winnings(int (*type)(const string&), vector<char>& cardOrder) {
  vector<Hand> newHands(hands.size());
  copy(hands.begin(), hands.end(), newHands.begin());
  sort(newHands.begin(), newHands.end(),
       [type, cardOrder](const Hand& hand1, const Hand& hand2) {
         int t1 = type(hand1.cards);
         int t2 = type(hand2.cards);
         if (t1 != t2) { return t1 > t2; }
         vector<int> v1, v2;
         for (char ch : hand1.cards) {
           v1.push_back(find(cardOrder.begin(), cardOrder.end(), ch) -
                        cardOrder.begin());
         }
         for (char ch : hand2.cards) {
           v2.push_back(find(cardOrder.begin(), cardOrder.end(), ch) -
                        cardOrder.begin());
         }
         return v1 < v2;
       });
  int ans = 0;
  for (int i = 0; i < (int)newHands.size(); ++i) {
    ans += newHands[i].bid * (i + 1);
  }
  return ans;
}

int part1() { return winnings(&type1, CARD_ORDER_1); }

int part2() { return winnings(&type2, CARD_ORDER_2); }

int main() {
  input();
  cout << "Part 1: " << part1() << endl;
  cout << "Part 2: " << part2() << endl;
  return 0;
}
