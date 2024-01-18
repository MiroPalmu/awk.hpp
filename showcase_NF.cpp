#include <iostream>
#include <print>
#include <sstream>
#include <string>

#include "awk.hpp"

const std::string csv =
    R"(1
1 2
1 2 3
1 2 3 4
1 2 3 4 5
1 2 3 4 5 6
1 2 3 4 5 6 7
1 2 3 4 5 6 7 8
1 2 3 4 5 6 7 8 9
1 2 3 4 5 6 7 8
1 2 3 4 5 6 7
1 2 3 4 5 6
1 2 3 4 5
1 2 3 4
1 2 3
1 2
1
)";

int main() {
    using namespace awk;

    std::println("For csv:\n");
    std::println("```\n{}\n```\n", csv);

    std::println("```\n{}\n```\n", "BEGIN{NF = 4} {print $0 $1 $2 $3 $4 $5 $6 $7 $8}");
    std::println("{}", csv | "BEGIN{NF = 7} {print $0 $1 $2 $3 $4 $5 $6 $7 $8}"_awk);

    std::println("```\n{}\n```\n",
                 "print 'NF =' NF '->' $0 $1 $2 $3 $4 $5 $6 $7 $8; NF += $3; NF -= $6");
    std::println("{}",
                 csv | "print 'NF =' NF '->' $0 $1 $2 $3 $4 $5 $6 $7 $8; NF += $3; NF -= $6"_awk);
}
