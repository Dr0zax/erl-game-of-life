%%--------------------------------------------------------------------
%% @doc  Functional Conway's Game of Life (safe for edges)
%%--------------------------------------------------------------------
-module(game_of_life).

-export([next_state/2, neighbors/3, next_grid/1, run/2, pretty_print/1, run_demo/2]).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

-spec next_state(integer(), integer()) -> integer().
next_state(1, Alive) when Alive < 2 ->
    0;   % underpopulation
next_state(1, 2) ->
    1;
next_state(1, 3) ->
    1;
next_state(1, Alive) when Alive > 3 ->
    0;   % overpopulation
next_state(0, 3) ->
    1;                      % reproduction
next_state(_, _) ->
    0.                      % dead otherwise

%% neighbors(Grid, RowIndexZeroBased, ColIndexZeroBased) -> [0|1]
-spec neighbors([[integer()]], non_neg_integer(), non_neg_integer()) -> [integer()].
neighbors(Grid, R, C) ->
    Offsets = neighbor_offsets(),
    lists:foldl(fun({DR, DC}, Acc) ->
                   NR = R + DR,
                   NC = C + DC,
                   case in_bounds(Grid, NR, NC) of
                       true -> [get(Grid, NR, NC) | Acc];
                       false -> Acc
                   end
                end,
                [],
                Offsets).

-spec next_grid([[integer()]]) -> [[integer()]].
next_grid(Grid) ->
    %% Map each row with its index
    map_with_index(Grid, fun(RowIndex, Row) -> next_row(Grid, RowIndex, Row) end).

-spec run([[integer()]], non_neg_integer()) -> [[integer()]].
run(Grid, 0) ->
    Grid;
run(Grid, N) when N > 0 ->
    Next = next_grid(Grid),
    run(Next, N - 1).

%% pretty_print prints grid with '#' for alive and '.' for dead
-spec pretty_print([[integer()]]) -> ok.
pretty_print(Grid) ->
    lists:foreach(fun(Row) -> io:format("~s~n", [row_to_string(Row)]) end, Grid),
    ok.

%% A small convenience demo that prints each generation up to Steps
-spec run_demo([[integer()]], non_neg_integer()) -> ok.
run_demo(Grid, Steps) when is_list(Grid), Steps >= 0 ->
    run_demo_loop(Grid, 0, Steps).

%% ------------------------------------------------------------------
%% Internal helpers
%% ------------------------------------------------------------------

%% Offsets for 8 neighbors (row_offset, col_offset)
-spec neighbor_offsets() -> [{integer(), integer()}].
neighbor_offsets() ->
    [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}].

%% Get value at row R col C (zero-based). Assumes in_bounds was checked by caller.
-spec get([[integer()]], non_neg_integer(), non_neg_integer()) -> integer().
get(Grid, R, C) ->
    Row = lists:nth(R + 1, Grid),      % lists:nth is 1-based
    lists:nth(C + 1, Row).

%% Check whether coordinates are inside the grid
-spec in_bounds([[integer()]], integer(), integer()) -> boolean().
in_bounds(Grid, R, C) ->
    RowCount = length(Grid),
    RowCount > 0
    andalso R >= 0
    andalso R < RowCount
    andalso case lists:nth(1, Grid) of
                Row ->
                    C >= 0 andalso C < length(Row)
            end.

%% Count alive neighbors (expects neighbors list of 0/1)
-spec count_alive([integer()]) -> non_neg_integer().
count_alive(Neighbors) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Neighbors).

%% Compute next state for a given row (RowIndex zero-based)
-spec next_row([[integer()]], non_neg_integer(), [integer()]) -> [integer()].
next_row(Grid, RowIndex, Row) ->
    map_with_index(Row,
                   fun(ColIndex, Cell) ->
                      AliveNeighbors = count_alive(neighbors(Grid, RowIndex, ColIndex)),
                      next_state(Cell, AliveNeighbors)
                   end).

%% map_with_index: applies Fun(IndexZeroBased, Element) to each element and returns list of results
-spec map_with_index([any()], fun((non_neg_integer(), any()) -> any())) -> [any()].
map_with_index(List, Fun) ->
    map_with_index(List, Fun, 0).

map_with_index([], _Fun, _Index) ->
    [];
map_with_index([H | T], Fun, I) ->
    [Fun(I, H) | map_with_index(T, Fun, I + 1)].

%% Convert a row [0,1,0] -> string ".#."
-spec row_to_string([integer()]) -> string().
row_to_string(Row) ->
    Chars = [cell_char(Cell) || Cell <- Row],
    lists:flatten([[C] || C <- Chars]).

-spec cell_char(integer()) -> char().
cell_char(1) ->
    $#;
cell_char(0) ->
    $.;
cell_char(_) ->
    $..  %% fallback

%% Demo loop printing generations
-spec run_demo_loop([[integer()]], non_neg_integer(), non_neg_integer()) -> ok.
run_demo_loop(_Grid, Gen, Steps) when Gen > Steps ->
    ok;
run_demo_loop(Grid, Gen, Steps) ->
    io:format("Generation ~p:~n", [Gen]),
    pretty_print(Grid),
    io:format("~n", []),
    Next = next_grid(Grid),
    run_demo_loop(Next, Gen + 1, Steps).

%% ------------------------------------------------------------------
%% Example usage (in shell):
%%
%% 1> c(game_of_life).
%% 2> Blinker = [[0,1,0],[0,1,0],[0,1,0]].
%% 3> game_of_life:pretty_print(Blinker).
%% 4> game_of_life:run(Blinker, 1) |> game_of_life:pretty_print().
%% 5> game_of_life:run_demo(Blinker, 4).
%%
%% Works for any rectangular grid. Safe at edges.
%% ------------------------------------------------------------------
