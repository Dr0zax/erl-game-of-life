erlGameOfLife
=====

An OTP application

Build
-----
1. Make sure erlang is installed
```bash
erl
```

- if you dont have it installed download it at https://www.erlang.org/patches/otp-26.2.5.16

2. compile in the erlang shell
```erlang
c(game_of_life)
```

Usage (while in erlang shell)
-----
### create a grid
```erlang
Blinker = [[0,1,0],[0,1,0],[0,1,0]].
```

### One Step Generation
```erlang
NextGen = game_of_life:run(Blinker, 1).
game_of_life:pretty_print(NextGen).
```

### Run multiple Generations
```erlang
game_of_life:run_demo(Blinker, 4).
```