-module(game_of_life).

-export([
    next_state/2,
    neighbors/3,
    run/2,
    pretty_print/1,
    run_demo/2
]).


% Rules of Conway's Game of Life

-spec next_state(integer(), integer()) -> integer().
next_state(1, Alive) when Alive < 2 -> 0; % Underpopulation
next_state(1, 2) -> 1; % Survival
next_state(1, 3) -> 1; % Survival
next_state(1, Alive) when Alive > 3 -> 0; % Overpopulation
next_state(0, 3) -> 1; % Reproduction
next_state(_, _) -> 0. % Remains dead

-spec neighbors(list(list(integer())), integer(), integer()) -> integer().
neighbors(Grid, R, C) ->
    Offsets = neighbor_offsets(),
    [get(Grid, NR, NC) || {DR, DC} <- Offsets,
     NR = R + DR,
     NC = C + DC,
     in_bounds(Grid, NR, NC)].

-spec next_grid([[integer()]]) -> [[integer()]].
next_grid(Grid) -> 
    map_with_index(Grid, fun(RowIndex, Row) -> next_grid(Grid, RowIndex, Row) end).
         