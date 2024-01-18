#include <chrono>
#include <concepts>
#include <cstddef>
#include <format>
#include <print>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "awk.hpp"

void time(const std::invocable<const std::string&> auto f,
          const std::invocable auto str_generator,
          const std::size_t amount_of_invocations,
          const std::string_view name) {
    // Generate data
    auto csv_vec = std::vector<std::string>(amount_of_invocations);
    for (std::size_t i = 0; i < amount_of_invocations; ++i) csv_vec[i] = str_generator();

    // Time
    const auto start{ std::chrono::steady_clock::now() };
    for (std::size_t i = 0; i < amount_of_invocations; ++i) volatile auto _ = f(csv_vec[i]);
    const auto end{ std::chrono::steady_clock::now() };
    const std::chrono::duration<double> elapsed{ end - start };
    const auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

    std::println("Calling {} {} times took {}", name, amount_of_invocations, elapsed_ms);
}

class fraction {
    double value_;

  public:
    fraction(const double d) : value_{ d } {
        if (d > 1.0 or d < 0.0)
            throw std::runtime_error{ std::format("{} is invalid fraction!", d) };
    }
    operator double() const { return value_; }
};

std::string random_str(const std::size_t min_length = 4, const std::size_t max_length = 10) {
    static auto gen   = std::mt19937{ std::random_device{}() };
    auto str_len_dist = std::uniform_int_distribution<std::size_t>{ min_length, max_length };
    auto char_dist    = std::uniform_int_distribution<std::size_t>{ 97, 112 };

    auto ss = std::stringstream{};
    for (std::size_t i = 0; i < str_len_dist(gen); ++i) { ss << static_cast<char>(char_dist(gen)); }
    return ss.str();
}

double random_number(const double min = -1.0, const double max = 1.0) {
    static auto gen  = std::mt19937{ std::random_device{}() };
    auto number_dist = std::uniform_real_distribution<double>{ min, max };

    return number_dist(gen);
}

std::string
    generate_csv(const fraction amount_of_str, const std::size_t rows, const std::size_t columns) {
    static auto gen = std::mt19937{ std::random_device{}() };
    auto str_distr  = std::uniform_real_distribution<double>{ 0.0, 1.0 };

    auto csv = std::stringstream{};
    for (std::size_t i = 0; i < rows; ++i) {
        for (std::size_t j = 0; j < rows - 1; ++j) {
            if (str_distr(gen) < amount_of_str) csv << random_str() << ", ";
            else
                csv << random_number() << ", ";
        }
        if (str_distr(gen) < amount_of_str) csv << random_str() << '\n';
        else
            csv << random_number() << '\n';
    }

    return csv.str();
}

int main(int argc, const char** argv) {
    using namespace awk;
    std::println("Running benchmarks name[fraction of strings, rows, columns]...");

    constexpr std::size_t columns    = 10;
    constexpr std::size_t total_rows = 500000;
    for (const auto frac_of_strings : { 0.1, 0.5, 0.9 }) {
        for (const auto rows : { 10, 20, 40 }) {
            const auto runtimes = total_rows / rows;

            time(
                [&](const std::string& csv) {
                    return csv | "a = ''; a += $5; print $1 $4 $7 $8 a"_awk;
                },
                [&] { return generate_csv(frac_of_strings, rows, columns); },
                runtimes,
                std::format("c[{}, {}, {}]", frac_of_strings, rows, columns));

            if (argc == 2) {
                time([&](const std::string& csv) { return csv | impl::awk_engine{ argv[1] }; },
                     [&] { return generate_csv(frac_of_strings, rows, columns); },
                     runtimes,
                     std::format("r[{}, {}, {}]", frac_of_strings, rows, columns));
            }
        }
    }
}
