#include <assert.h>
#include <stdio.h>
#include <string.h>

#define MAX_LINES 1000
#define M 7

// Encoding each segment as a different bit:
// gfedcba
// 1110111 -> 0
const int digits[10] = {
    119, // 1110111
    36,  // 0100100
    93,  // 1011101
    109, // 1101101
    46,  // 0101110
    107, // 1101011
    123, // 1111011
    37,  // 0100101
    127, // 1111111
    111  // 1101111
};

int valid[M];

int bit_count(int num) {
  int ans = 0;
  while (num) {
    if (num & 1)
      ++ans;
    num >>= 1;
  }
  return ans;
}

void init() {
  for (int i = 0; i < M; ++i)
    valid[i] = (1 << M) - 1;
  for (int i = 0; i < 10; ++i)
    valid[bit_count(digits[i])] &= digits[i];
}

int graph[M][M];
int seen[M];
int matchL[M], matchR[M];

int bpm(int u) {
  for (int v = 0; v < M; ++v)
    if (graph[u][v]) {
      if (seen[v])
        continue;
      seen[v] = 1;

      if (matchR[v] < 0 || bpm(matchR[v])) {
        matchL[u] = v;
        matchR[v] = u;
        return 1;
      }
    }
  return 0;
}

void match() {
  memset(matchL, -1, sizeof(matchL));
  memset(matchR, -1, sizeof(matchR));

  int cnt = 0;
  for (int i = 0; i < M; ++i) {
    memset(seen, 0, sizeof(seen));
    if (bpm(i))
      ++cnt;
  }

  assert(cnt == M);
}

int convert(char *str) {
  int ans = 0, i = 0;
  while (str[i]) {
    ans |= (1 << (str[i] - 'a'));
    ++i;
  }
  return ans;
}

int lines;
int patterns[10];
int output[4];
int ans[MAX_LINES];

int go() {
  memset(graph, 0, sizeof(graph));
  int possible[M];
  for (int i = 0; i < M; ++i)
    possible[i] = (1 << M) - 1;
  for (int i = 0; i < 10; ++i) {
    int m = patterns[i];
    int cnt1 = bit_count(m);
    for (int j = 0; j < 9; ++j) {
      int num = digits[j];
      int cnt2 = bit_count(num);
      if (cnt1 != cnt2)
        continue;
      for (int k = 0; k < M; ++k)
        if (valid[cnt2] & (1 << k))
          possible[k] &= m;
    }
  }

  for (int r = 0; r < M; ++r)
    for (int l = 0; l < M; ++l)
      if (possible[r] & (1 << l))
        graph[l][r] = 1;

  match();

  int ans = 0;
  for (int i = 0; i < 4; ++i) {
    ans *= 10;
    int wires = 0;
    for (int bit = 0; bit < M; ++bit)
      if (output[i] & (1 << bit))
        wires |= (1 << matchL[bit]);
    for (int tgt = 0; tgt < 10; ++tgt) {
      if (digits[tgt] == wires) {
        ans += tgt;
        break;
      }
    }
  }
  return ans;
}

int main(int argc, const char **argv) {
  init();

  lines = 0;
  FILE *file = fopen("inputs/08.input", "r");
  char line[14][10];
  while (fscanf(file, "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s\n", line[0],
                line[1], line[2], line[3], line[4], line[5], line[6], line[7],
                line[8], line[9], line[10], line[11], line[12],
                line[13]) != EOF) {
    for (int i = 0; i < 10; ++i)
      patterns[i] = convert(line[i]);
    for (int i = 0; i < 4; ++i)
      output[i] = convert(line[i + 10]);
    ans[lines] = go();
    ++lines;
  }
  fclose(file);

  int part1 = 0;
  int part2 = 0;

  for (int i = 0; i < lines; ++i) {
    int num = ans[i];
    part2 += num;
    while (num) {
      int digit = num % 10;
      if (digit == 1 || digit == 4 || digit == 7 || digit == 8)
        ++part1;
      num /= 10;
    }
  }

  printf("Part 1: %d\n", part1);
  printf("Part 2: %d\n", part2);

  return 0;
}
