# awk.hpp
~Zero-overhead minimal awk library, meant as proof of concept and not for actual use,
i.e. it is not tested, and there are no plans to continue developing it.
~Zero-overhead means that the awk program is parsed and compiled to implementation detail
byte code during compile time. This byte code is stored to executable which then is
executed during runtime.

# Example usage

Awk engine is created using UDL `_awk` which is usable after `using namespace awk`
, e.g. `"{ print $0 }"_awk`.

Then one can pipe `std::string` to it using `operator|`, in which the execution happens.
It then returns the output from awk as `std::string`.

```cpp
const auto str = std::string{ "1 2 3\n4 5 6" };
std::println("{}", str | "a += $1; print a $2"_awk);
// Outputs:
//
// 2 3
// 7 6
```

# Requirements

C++23 conforming compiler (tested on gcc 14.0.1 20240115).

# Building showcase executables

Specific compiler can be given with `CXX` enviroment variable.

```bash
make
```

# Spesification

At the moment only small subset of awk features are implemented
and the ones that are implemented do not follow closely the original awk.

## Blocks

Awk program consists of three different blocks: BEGIN, COMMAND and END.
Each block consists of different commands separated with semicolon (`;`),
for the last command semicolon is optional.
The commands are executed in order from left to right.
BEGIN block is executed once in the beginning of program and END at end of program.
Commands in the COMMAND block are executed for each line of input (*).

Each block has to be surrounded using curly brackets (`{cmd1;cmd2;...}`).
Blocks are identified by amount of the blocks. If the amount of blocks is:

- *One*, then it is COMMAND block and then curly brackets are optional.
- *Two*, then the first block is COMMAND block and the second is END block,
   except if there is `BEGIN` string before the first block
   then the first block is BEGIN block and the second is COMMAND block.
- *Three* then the first is BEGIN block, the second is COMMAND block and
  the third is the END block.
- *Anything else* will not compile.

Everything outside of the blocks except the `BEGIN` string are ignored.

## Variables and literals

All the variables are of type number or string
and they can be accessed using the name of variable in any part of the program,
if the variable does not exists then it is created as empty string.

If a variable name can be parsed as number then it is a number.
Numbers are in form `[-]A[.]B` where square brackets denote optionality and
`A` and `B` are zero or more characters in range `[0-9]` but such that
at least other has to be present.

String literals can be created by enclosing them with ' characters, e.g. `'I am a string'`.
Escaping ' character in string is not implemented and due to hacks used in implementation,
if `@@STRING@@` appears anywhere in awk program, it is undefined behaviour.

There is three special variables which are always defined:
- `FS`
    - default: `', '`
    - setting to a number and using it results in thrown exception
- `OFS`
    - default: `' '`
- `NF`
    - default: `1`
    - setting to a string and using it results in thrown exception

## Fields

Beginning of execution of any block special variables are set called fields
based on the line which is currently being read from the input. During the execution
of BEGIN and END blocks, this line is empty.

The characters in `FS` are called field separators. The current line is split at
any field separator (consecutive field separators are treated as one field separator),
and the resulting pieces are stored to fields.
If the piece can be parsed as a number then it is stored as number otherwise as string.

Fields are indexed from left to right starting from zero.
Fields can be accessed using `$n` where n is the index of the field.
Similar to variables if field does not exists it is an empty string.

(*) `NF` indicates the minimum number of fields that line has to contain,
otherwise that line is skipped.

## Commands

List of implemented commands where `var` is a variable name
and the white space matters:

- `var = val`
    - store `val` to `var` regardless of type of `val`
- `var += val`
    - store `var + val` to `var`
    - if both `var` and `val` are strings, then `val` is appended to `var`
- `var -= val`
    - store `var - val` to `var`
    - if both `var` and `val` are strings, then exception is thrown
- `print arg_1 ... arg_n`
    - print all arguments and with line break at the end
    - print `OFS` between every argument

There is special rule that in addition and subtraction if the other value
is a number and the other is a empty string then the empty string is treated
as a number with value of zero.

*Note* that arithmetics is not implemented. Only way to add or subtract numbers
is with `+=` or `-=`.
