#include <print>
#include <string>
#include <iostream>
#include <sstream>

#include "awk.hpp"

const std::string csv =
R"(A B C|[]|E F G
A B C|&&|E F G
A B C|~~|E F G)";


int main() {
    using namespace awk;

    std::println("For csv:\n");
    std::println("```\n{}\n```\n", csv);

    std::println("```\n{}\n```\n", "BEGIN{FS = '|'}{OFS = $1; print $0 $2}");
    std::println("{}", csv | "BEGIN{FS = '|'}{OFS = $1; print $0 $2}"_awk);

    std::println("```\n{}\n```\n", "{FS = '|'}{print 'appended' $1 'to' m; m += $1}{print m}");
    std::println("{}", csv | "{FS = '|'}{print 'appended' $1 'to' m; m += $1}{print m}"_awk);
}
