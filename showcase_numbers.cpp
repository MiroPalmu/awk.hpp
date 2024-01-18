#include <iostream>
#include <print>
#include <sstream>
#include <string>

#include "awk.hpp"

const std::string csv =
    R"(-1, -2, 3
1.2, 3.4, 0
0.1, -3.2, -2)";

int main() {
    using namespace awk;

    std::println("For csv:\n");
    std::println("```\n{}\n```\n", csv);

    std::println("```\n{}\n```\n",
                 "{a += $0; b += $1; c += $2} {a += c; a -= b; print 'result:' a}");
    std::println("{}", csv | "{a += $0; b += $1; c += $2} {a += c; a -= b; print 'result:' a}"_awk);

    std::println("```\n{}\n```\n", "a = $0; a += $1; a += $2; print a");
    std::println("{}", csv | "a = $0; a += $1; a += $2; print a"_awk);

    std::println("```\n{}\n```\n", "BEGIN{OFS = '|'}{print $2 $0; OFS += '|'}");
    std::println("{}", csv | "BEGIN{OFS = '|'}{print $2 $0; OFS += '|'}"_awk);
}
