#include <print>
#include <string>
#include <iostream>
#include <sstream>

#include "awk.hpp"

int main() {
    using namespace awk;
    using namespace std::literals;
    std::println("```\n{}\n```\n", "using namespace awk;\nusing namespace std::literals;");

    const auto code = R"(constexpr auto growth = "n = $0; a = $1; print n a; n += 1; a += $1; print n a"_awk;
std::println("{}", "0 x"s | growth | growth | growth | growth | growth);)"s;

    std::println("```\n{}\n```\n", code);
    constexpr auto growth = "n = $0; a = $1; print n a; n += 1; a += $1; print n a"_awk;
    std::println("{}", "0 x"s | growth | growth | growth | growth | growth);
}
