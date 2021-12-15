program Day09;

const
   INPUT                             = 'inputs/09.input';
   MAX_SIZE                          = 1000;
   DIRS : array[0..3, 0..1] of Int32 = ((0, 1), (1, 0), (0, -1), (-1, 0));

var
   tfIn       : TextFile;
   s          : string;
   grid       : array[0..MAX_SIZE, 0..MAX_SIZE] of Int32;
   visited    : array[0..MAX_SIZE, 0..MAX_SIZE] of boolean;
   top_basins : array[0..2] of Int32;
   H          : Int32;
   W          : Int32;
   i          : Int32;
   j          : Int32;
   curr_basin : Int32;
   part1      : Int32;
   part2      : Int32;

function valid_coord(i, j : Int32): boolean;
begin
   valid_coord := (i >= 0) and (i < H) and (j >= 0) and (j < W);
end;

function risk_level(i, j : Int32): Int32;
begin
   risk_level := grid[i, j] + 1;
end;

function is_low_point(i, j : Int32): boolean;
var
   d  : Int32;
   ni : Int32;
   nj : Int32;
begin
   is_low_point := true;
   for d := 0 to 3 do
   begin
      ni := i + DIRS[d, 0];
      nj := j + DIRS[d, 1];
      if valid_coord(ni, nj) and (grid[ni, nj] <= grid[i, j]) then
         is_low_point := false;
   end;
end;

function basin(i, j : Int32): Int32;
var
   d  : Int32;
   ni : Int32;
   nj : Int32;
begin
   visited[i, j] := true;
   basin := 1;
   for d := 0 to 3 do
   begin
      ni := i + DIRS[d, 0];
      nj := j + DIRS[d, 1];
      if valid_coord(ni, nj) and (visited[ni, nj] = false) and (grid[ni, nj] <> 9) and (grid[ni, nj] > grid[i, j]) then
         basin := basin + basin(ni, nj);
   end;
end;

function sort_basins(): boolean;
var
   tmp : Int32;
begin
   sort_basins := false;

   if (top_basins[0] > top_basins[1]) then
   begin
      tmp := top_basins[0];
      top_basins[0] := top_basins[1];
      top_basins[1] := tmp;
      sort_basins := true;
   end;

   if (top_basins[0] > top_basins[2]) then
   begin
      tmp := top_basins[0];
      top_basins[0] := top_basins[2];
      top_basins[2] := tmp;
      sort_basins := true;
   end;

   if (top_basins[1] > top_basins[2]) then
   begin
      tmp := top_basins[1];
      top_basins[1] := top_basins[2];
      top_basins[2] := tmp;
      sort_basins := true;
   end;
end;

begin
   Assign(tfIn, INPUT);

   reset(tfIn);
   H := 0;
   while not eof(tfIn) do
   begin
      readln(tfIn, s);
      for W := 0 to (length(s) - 1) do
      begin
         grid[H, W] := ord(s[W + 1]) - ord('0');
         visited[H, W] := false;
      end;
      H := H + 1;
   end;
   W := W + 1;
   Close(tfIn);

   top_basins[0] := 0;
   top_basins[1] := 0;
   top_basins[2] := 0;

   part1 := 0;
   for i := 0 to (H - 1) do
      for j := 0 to (W - 1) do
         if is_low_point(i, j) then
         begin
            curr_basin := basin(i, j);
            if (curr_basin > top_basins[0]) then
            begin
               top_basins[0] := curr_basin;
               sort_basins();
            end;
            part1 := part1 + risk_level(i, j);
         end;

   write('Part 1: ');
   writeln(part1);

   part2 := top_basins[0] * top_basins[1] * top_basins[2];
   write('Part 2: ');
   writeln(part2);
end.
