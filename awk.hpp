#pragma once
#include <algorithm>
#include <array>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <print>
#include <ranges>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>

namespace awk {
// First there is some helper:

template<std::size_t max_length>
struct fixed_string {
    std::array<char, max_length + 1> data_ = {};

    [[nodiscard]] explicit constexpr fixed_string(const std::string_view input) {
        std::ranges::copy(input, data_.data());
    }

    template<std::size_t N>
        requires(N <= max_length)
    [[nodiscard]] constexpr fixed_string(const char (&input)[N])
        : fixed_string(static_cast<std::string_view>(input)) {}

    [[nodiscard]] constexpr std::string_view sv() const { return std::string_view(data_.begin()); }
};

constexpr std::size_t parse_size_t(const std::string_view str) {
    std::size_t result = 0;

    auto is_digit = [](const char c) { return c >= '0' && c <= '9'; };

    for (const auto c : str) {
        if (!is_digit(c)) { return std::numeric_limits<int>::quiet_NaN(); }
        result = result * 10 + (c - '0');
    }

    return result;
}

constexpr std::optional<double> parse_number(const std::string_view str) {
    if (str.empty()) return {};
    double result = 0;

    const auto minus = str.front() == '-';

    auto is_digit = [](const char c) { return c >= '0' && c <= '9'; };
    auto decimals = double{ 0 };

    for (const auto c : str | std::views::drop(minus)) {
        if (c == '.') {
            if (decimals) // If another period
                return {};

            decimals = 0.1;
            continue;
        }

        if (!is_digit(c)) { return {}; }

        if (not decimals) {
            result = result * 10 + (c - '0');
        } else {
            result   = result + (c - '0') * decimals;
            decimals = 0.1 * decimals;
        }
    }
    if (minus) return -minus;
    else
        return result;
}

// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
// explicit deduction guide (not needed as of C++20)
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

/// Quotes are denoted with ' not ". Returns things that are not in quotes and
/// ten quotes
constexpr auto snip_quotes(const std::string_view str)
    -> std::tuple<std::vector<std::string>, std::vector<std::string>> {
    auto indecies_of_quotes = std::vector<std::size_t>{};
    for (const auto [i, c] : str | std::views::enumerate) {
        if (c == '\'') indecies_of_quotes.push_back(i);
    }

    if (indecies_of_quotes.size() % 2 != 0) throw std::logic_error{ "Unmatched quotes (')!" };

    auto outside_quotes = std::vector<std::string>{};
    auto quotes         = std::vector<std::string>{};

    auto one_past_last_rhs = std::size_t{ 0 };
    for (const auto chunk : indecies_of_quotes | std::views::chunk(2)) {
        const auto lhs_index = chunk[0];
        const auto rhs_index = chunk[1];

        outside_quotes.push_back({ &str[one_past_last_rhs], &str[lhs_index] });

        quotes.push_back({ &str[lhs_index + 1], &str[rhs_index] });

        one_past_last_rhs = rhs_index + 1;
    }
    outside_quotes.push_back({ &str[one_past_last_rhs], str.end() });
    return { outside_quotes, quotes };
}

// Helpers end here and awk implementation begins:

// awk_string -> parser -> {begin,command,end}-tokens -> sema -> awk_program ->
// {begin,commands,end}-instructions in awk_engine

struct null {};
struct semicolon {};
struct print_token_t {};
struct field {
    std::size_t n;
};
struct unclassified_word {
    std::string str;
};
struct string_literal {
    std::string str;
};
struct equal {};
struct equal_plus {};
struct equal_minus {};

using token = std::variant<null,
                           semicolon,
                           print_token_t,
                           field,
                           unclassified_word,
                           string_literal,
                           equal,
                           equal_plus,
                           equal_minus>;

enum class token_flags : std::size_t {
    null_e              = 1 << 0,
    semicolon_e         = 1 << 1,
    print_e             = 1 << 2,
    field_e             = 1 << 3,
    unclassified_word_e = 1 << 4,
    string_literal_e    = 1 << 5,
    equal_e             = 1 << 6,
    equal_plus_e        = 1 << 7,
    equal_minus_e       = 1 << 8
};

constexpr auto internals_of_curly_brackets(const std::string_view str)
    -> std::vector<std::string_view> {
    auto lhs_indecies = std::vector<std::string_view::size_type>{};
    auto rhs_indecies = std::vector<std::string_view::size_type>{};

    for (const auto [i, c] : str | std::views::enumerate) {
        switch (c) {
            case '{': lhs_indecies.push_back(i); break;
            case '}': rhs_indecies.push_back(i); break;
        }
    }

    if (lhs_indecies.size() != rhs_indecies.size()) throw std::logic_error{ "Unmatched {}!" };

    auto brackets = std::vector<std::string_view>{};
    auto one_past_previous_rhs =
        std::size_t{ 0 }; // This is used to check that the { } are not overlapping like { {} }
    for (const auto [lhs_index, rhs_index] : std::views::zip(lhs_indecies, rhs_indecies)) {
        if (lhs_index >= rhs_index) throw std::logic_error{ "} comes before {!" };
        if (lhs_index < one_past_previous_rhs) throw std::logic_error{ "nested {}!" };
        one_past_previous_rhs = rhs_index + 1;

        brackets.emplace_back(&str[lhs_index + 1], &str[rhs_index]);
    }

    return brackets;
}

constexpr auto contains_BEGIN_before_first_curly_bracket(const std::string_view str) {
    const auto begin_should_be_here = std::string_view{ str.begin(), str.find_first_of('{') };
    return begin_should_be_here.contains("BEGIN");
}

constexpr auto get_tokens(const std::string_view commands) -> std::vector<token> {
    using namespace std::literals;
    auto all_tokens = std::vector<token>{};

    const auto [outside_quotes, quotes] = snip_quotes(commands);
    // We know that outside_quotes.size() == quotes.size() + 1

    // Replace quoted strings with @@STRING@@ and use it to parse string
    auto commands_quotes_removed = std::string{};
    for (const auto& out :
         outside_quotes | std::views::take(std::ranges::size(outside_quotes) - 1)) {
        commands_quotes_removed += out + " @@STRING@@ ";
    };
    commands_quotes_removed += outside_quotes.back();

    auto next_quote = 0uz;

    for (const auto command : commands_quotes_removed | std::views::split(';')) {
        if (command.empty()) continue;

        for (const auto word : command | std::views::split(' ')) {
            // Treat , as optional to separate arguments
            if (word.empty() or std::ranges::equal(word, ","sv)) continue;

            if (word.front() == '$') {
                all_tokens.push_back(
                    field{ parse_size_t({ std::next(word.begin()), word.end() }) });
            } else if (std::ranges::equal(word, "print"sv)) {
                all_tokens.push_back(print_token_t{});
            } else if (std::ranges::equal(word, "+"sv)) {
                // all_tokens.push_back(plus{});
                throw std::logic_error{ "Arithmetics are not supported, only [+-]= is supported!" };
            } else if (std::ranges::equal(word, "-"sv)) {
                // all_tokens.push_back(minus{});
                throw std::logic_error{ "Arithmetics are not supported, only [+-]= is supported!" };
            } else if (std::ranges::equal(word, "="sv)) {
                all_tokens.push_back(equal{});
            } else if (std::ranges::equal(word, "+="sv)) {
                all_tokens.push_back(equal_plus{});
            } else if (std::ranges::equal(word, "-="sv)) {
                all_tokens.push_back(equal_minus{});
            } else if (std::ranges::equal(word, "@@STRING@@"sv)) {
                all_tokens.push_back(string_literal{ quotes[next_quote++] });
            } else {
                all_tokens.push_back(unclassified_word{ { word.begin(), word.end() } });
            }
        }

        all_tokens.push_back(semicolon{});
    }

    return all_tokens;
}

/// Returns tokens from beginning, command and end block
constexpr auto get_all_tokens(const std::string_view input)
    -> std::tuple<std::vector<token>, std::vector<token>, std::vector<token>> {
    if (input.contains("\n\t")) throw std::logic_error{ "New lines and tabs are not supported!" };

    const auto curly_brackets = internals_of_curly_brackets(input);
    // Split program to begin, command, end
    auto begin   = std::vector<token>{};
    auto command = std::vector<token>{};
    auto end     = std::vector<token>{};

    switch (curly_brackets.size()) {
        case 0: command = get_tokens(input); break;
        case 1: command = get_tokens(curly_brackets[0]); break;
        case 2: {
            if (contains_BEGIN_before_first_curly_bracket(input) /* Hack */) {
                begin   = get_tokens(curly_brackets[0]);
                command = get_tokens(curly_brackets[1]);
            } else {
                command = get_tokens(curly_brackets[0]);
                end     = get_tokens(curly_brackets[1]);
            }
            break;
        }
        case 3: {
            begin   = get_tokens(curly_brackets[0]);
            command = get_tokens(curly_brackets[1]);
            end     = get_tokens(curly_brackets[2]);
            break;
        }
        default: throw std::logic_error{ "More than 3 curly brackets!" };
    }

    return { begin, command, end };
}

/// Debugging purposes:
void print_tokens(const std::span<token const> tokens) {
    auto token_printer =
        overloaded{ [](semicolon) { std::println(";"); },
                    [](print_token_t) { std::print("print:"); },
                    [](const field f) { std::print("${}", f.n); },
                    [](const unclassified_word& uw) { std::print("uw({})", uw.str); },
                    [](const string_literal& sl) { std::print("sl({})", sl.str); },
                    [](equal) { std::print("="); },
                    [](equal_plus) { std::print("+="); },
                    [](equal_minus) { std::print("-="); },
                    [](null) { std::print("NULL"); } };

    for (const auto& token : tokens) {
        std::visit(token_printer, token);
        std::print(" ");
    }
    std::println("");
}

// Forward declerations for friends
class awk_engine;
class awk_ast;
constexpr std::vector<std::string> get_all_variable_names(const std::span<awk_ast const> asts);

class awk_ast {
    friend awk_engine;
    friend constexpr std::vector<std::string>
        get_all_variable_names(const std::span<awk_ast const> asts);

    struct variable {
        std::string name;
    };
    using expression = std::variant<variable, string_literal, double, field>;

    struct assignment {
        variable var;
        expression expr;
        bool add;
        bool with_negation;
    };

    struct print_command {
        std::vector<expression> args;
    };

    using cmd = std::variant<assignment, print_command>;

    std::vector<cmd> commands_;

    // Takes all tokens of command without semicolon
    constexpr void handle_one_command(const std::span<token const> tokens) {
        if (tokens.empty()) return;
        if (tokens.size() < 2) throw std::logic_error{ "Command with just one token!" };

        auto print_cmd      = std::optional<print_command>{};
        auto assignment_cmd = std::optional<assignment>{};

        auto first_token_visitor = overloaded{
            [&](print_token_t) {
                auto local_print_cmd = print_command{};
                auto print_arg_visitor =
                    overloaded{ [&](const unclassified_word& uw) {
                                   const auto num = parse_number(uw.str);
                                   if (num.has_value()) {
                                       local_print_cmd.args.push_back(num.value());
                                   } else {
                                       local_print_cmd.args.push_back(variable{ uw.str });
                                   }
                               },
                                [&](const field& f) { local_print_cmd.args.push_back(f); },
                                [&](const string_literal& s) { local_print_cmd.args.push_back(s); },
                                [](auto) {
                                    throw std::logic_error{ "Printing only supports: fields, "
                                                            "variables and string literals!" };
                                } };

                std::ranges::for_each(tokens | std::views::drop(1),
                                      [&](auto& x) { std::visit(print_arg_visitor, x); });
                print_cmd = local_print_cmd;
            },
            [&](const unclassified_word& uw) {
                const auto num = parse_number(uw.str);
                if (num.has_value()) throw std::logic_error{ "Expecting variable not number!" };

                if (tokens.size() != 3)
                    throw std::logic_error{
                        "Only supported assignments are in form 'var [+-]= val'!"
                    };

                auto local_assignment_cmd = assignment{ variable{ uw.str }, {}, false, false };

                auto assignment_visitor =
                    overloaded{ [&](equal) {},
                                [&](equal_plus) { local_assignment_cmd.add = true; },
                                [&](equal_minus) {
                                    local_assignment_cmd.add           = true;
                                    local_assignment_cmd.with_negation = true;
                                },
                                [](auto) {
                                    throw std::logic_error{ "Was expecting assignment!" };
                                } };

                auto assignment_arg_visitor =
                    overloaded{ [&](const unclassified_word& uw) {
                                   const auto num = parse_number(uw.str);
                                   if (num.has_value()) {
                                       local_assignment_cmd.expr = num.value();
                                   } else {
                                       local_assignment_cmd.expr = variable{ uw.str };
                                   }
                               },
                                [&](const field& f) { local_assignment_cmd.expr          = f; },
                                [&](const string_literal& s) { local_assignment_cmd.expr = s; },
                                [](auto) {
                                    throw std::logic_error{ "Assingment only supports: fields, "
                                                            "variables and string literals!" };
                                } };

                std::visit(assignment_visitor, tokens[1]);
                std::visit(assignment_arg_visitor, tokens[2]);
                assignment_cmd = local_assignment_cmd;
            },
            [](auto) { throw std::logic_error{ "Was expecting print or assignment!" }; }
        };

        std::visit(first_token_visitor, tokens[0]);

        if (print_cmd.has_value()) commands_.push_back(print_cmd.value());
        else if (assignment_cmd.has_value())
            commands_.push_back(assignment_cmd.value());
        else
            throw std::logic_error{ "This should happend :) " };
    }

  public:
    constexpr awk_ast(const std::span<token const> tokens) {
        auto current_cmd_begin = std::size_t{ 0 };

        for (const auto [i, token] : tokens | std::views::enumerate) {
            if (std::holds_alternative<semicolon>(token)) {
                handle_one_command(tokens.subspan(current_cmd_begin, i - current_cmd_begin));
                current_cmd_begin = i + 1;
            }
        }
    }
};

constexpr std::vector<std::string> get_all_variable_names(const std::span<awk_ast const> asts) {
    auto names = std::vector<std::string>{};

    auto append_unique_name = [&](const std::string& s) {
        if (std::ranges::contains(names, s)) return;
        else
            names.push_back(s);
    };

    // Index 0 is reserved for FS
    append_unique_name("FS");
    // and 1 for OFS
    append_unique_name("OFS");

    auto append_names =
        overloaded{ [&](const awk_ast::assignment& a) {
                       append_unique_name(a.var.name);
                       if (std::holds_alternative<awk_ast::variable>(a.expr)) {
                           append_unique_name(std::get<awk_ast::variable>(a.expr).name);
                       }
                   },
                    [&](const awk_ast::print_command& a) {
                        for (const auto& arg : a.args) {
                            if (std::holds_alternative<awk_ast::variable>(arg)) {
                                append_unique_name(std::get<awk_ast::variable>(arg).name);
                            }
                        }
                    } };

    for (const auto& ast : asts) {
        for (const auto& c : ast.commands_) { std::visit(append_names, c); }
    }

    return names;
}

class awk_engine {
    fixed_string<100> awk_code_;

    /// Prints n next data
    struct print_n {
        std::size_t n;
    };
    /// Copy next data to one after that (overwrite)
    struct copy {};
    /// Adds next two data and stores to third
    struct add {};
    /// Subtracts next two data and stores to third
    struct sub {};

    /// String literal is searched for in the awk strig
    struct str_data {
        const char* begin;
        const char* end;

        operator std::string_view() const { return std::string_view{ begin, end }; }
    };
    struct var_index {
        std::size_t i;
    };

    /// double is awk number, size_t is variable index and str_data is string
    /// literal;
    using data = std::variant<double, field, var_index, str_data>;

    /// Used to null terminate execution
    struct null {};

    using byte_code = std::variant<print_n, copy, add, sub, data, null>;

    constexpr auto get_byte_code(const awk_ast& ast, const std::span<std::string const> names) {
        auto code = std::vector<byte_code>{};

        auto get_data =
            overloaded{ [&](const awk_ast::variable& v) {
                           const auto name = v.name;
                           const auto i    = static_cast<std::size_t>(std::ranges::find(names, name)
                                                                   - names.begin());
                           return data{ var_index{ i } };
                       },
                        [&](const string_literal& sl) {
                            const auto begin_index = awk_code_.sv().find(sl.str);
                            const auto b           = std::next(awk_code_.sv().begin(), begin_index);
                            const auto e           = std::next(b, sl.str.size());
                            return data{ str_data{ b, e } };
                        },
                        [&](const double d) { return data{ d }; },
                        [&](const field f) { return data{ f }; } };

        auto visitor = overloaded{ [&](const awk_ast::assignment& a) {
                                      if (a.add) {
                                          // Assignment with addition
                                          if (a.with_negation) {
                                              code.push_back(sub{});
                                          } else {
                                              code.push_back(add{});
                                          }
                                          code.push_back(get_data(a.var));
                                          code.push_back(std::visit(get_data, a.expr));
                                          code.push_back(get_data(a.var));

                                      } else {
                                          // Normal assignment
                                          code.push_back(copy{});
                                          code.push_back(std::visit(get_data, a.expr));
                                          code.push_back(get_data(a.var));
                                      }
                                  },
                                   [&](const awk_ast::print_command& a) {
                                       code.push_back(print_n{ a.args.size() });
                                       for (const auto& e : a.args) {
                                           code.push_back(std::visit(get_data, e));
                                       }
                                   } };

        for (const auto& cmd : ast.commands_) { std::visit(visitor, cmd); }

        return code;
    }

    static constexpr std::size_t max_length = 40;
    std::array<byte_code, max_length + 1> begin_code_;
    std::array<byte_code, max_length + 1> command_code_;
    std::array<byte_code, max_length + 1> end_code_;

  public:
    constexpr awk_engine(const std::string_view input) : awk_code_{ input } {
        // Init bytecode to null
        begin_code_.fill(null{});
        command_code_.fill(null{});
        end_code_.fill(null{});

        const auto [b, c, e] = get_all_tokens(input);
        const auto b_ast     = awk_ast{ b };
        const auto c_ast     = awk_ast{ c };
        const auto e_ast     = awk_ast{ e };

        const auto unique_variable_names =
            get_all_variable_names(std::vector{ b_ast, c_ast, e_ast });

        const auto b_byte_code = get_byte_code(b_ast, unique_variable_names);
        const auto c_byte_code = get_byte_code(c_ast, unique_variable_names);
        const auto e_byte_code = get_byte_code(e_ast, unique_variable_names);

        if (b_byte_code.size() > max_length)
            throw std::logic_error{ "Too long bytecode for BEGIN!" };
        if (c_byte_code.size() > max_length)
            throw std::logic_error{ "Too long bytecode for COMMAND!" };
        if (e_byte_code.size() > max_length) throw std::logic_error{ "Too long bytecode for END!" };

        std::ranges::copy(b_byte_code, begin_code_.begin());
        std::ranges::copy(c_byte_code, command_code_.begin());
        std::ranges::copy(e_byte_code, end_code_.begin());
    }

    /// For debugging
    void print_byte_code() const {
        const auto data_printer =
            overloaded{ [](const double d) { std::println("\tNumber: {}", d); },
                        [](const field f) { std::println("\tField: {}", f.n); },
                        [](const var_index v) { std::println("\tVariable: {}", v.i); },
                        [](const str_data s) {
                            std::println("\tString: {}", static_cast<std::string_view>(s));
                        } };

        auto done = false;

        const auto printer = overloaded{ [](const print_n p) { std::println("Print {}:", p.n); },
                                         [](copy) { std::println("Copy:"); },
                                         [](add) { std::println("Add:"); },
                                         [](sub) { std::println("Sub:"); },
                                         [&](const data& d) { std::visit(data_printer, d); },
                                         [&](null) { done = true; } };

        std::println("BEGIN:");
        for (std::size_t i = 0; not done; ++i) { std::visit(printer, begin_code_[i]); }
        done = false;
        std::println("COMMAND:");
        for (std::size_t i = 0; not done; ++i) { std::visit(printer, command_code_[i]); }
        done = false;
        std::println("END");
        for (std::size_t i = 0; not done; ++i) { std::visit(printer, end_code_[i]); }
    }

    friend std::string operator|(const std::string_view str, const awk_engine& awk) {
        auto output = std::stringstream{};

        using var_t        = std::variant<std::string, double>;
        auto var_t_printer = overloaded{ [&](const std::string s) { output << s; },
                                         [&](const double d) { output << d; } };

        auto current_fields = std::vector<var_t>{};
        auto get_field      = [&](const std::size_t i) -> std::optional<var_t> {
            if (i >= current_fields.size()) return {};
            else
                return current_fields[i];
        };

        // Index 0 was reserved for FS and 1 for OFS
        using namespace std::literals;
        // Use empty string as "empty" variable
        auto memory       = std::vector<var_t>{ " \t"s, " "s };
        auto get_variable = [&](const std::size_t i) -> var_t {
            // Fill until we find what we are looking for
            while (memory.size() <= i) memory.push_back(""s);
            // Or it is already in memory
            return memory[i];
        };

        auto set_variable = [&](const std::size_t i, const var_t x) {
            // Fill until we find what we are looking for
            while (memory.size() <= i) memory.push_back(""s);
            // Or it is already in memory
            memory[i] = x;
        };
        auto set_variable_from_data = [&](const std::size_t i, const data x) {
            auto visitor =
                overloaded{ [&](const double d) { set_variable(i, d); },
                            [&](const field f) { set_variable(i, get_field(f.n).value_or(""s)); },
                            [&](const var_index v) { set_variable(i, get_variable(v.i)); },
                            // Ugliest of the ugly
                            [&](const str_data s) {
                                set_variable(i, std::string{ std::string_view{ s } });
                            } };
            std::visit(visitor, x);
        };

        auto set_fields = [&](std::string line) {
            current_fields.clear();
            // FS (index 0) should always be string, if it is not then... idk
            const auto FS = std::get<std::string>(get_variable(0));

            // Projects all but characters in FS to - and ones in FS to +
            auto project = [&](const char c) {
                if (FS.contains(c)) return '+';
                else
                    return '-';
            };

            auto comp = [](const char lhs, const char rhs) {
                // We want only collapse the FS characters which are marked with +
                return (lhs == '+') and (rhs == '+');
            };

            std::ranges::unique(line, comp, project);
            // Now there must be at most one FS in row

            auto begin_index = line.find_first_not_of(FS);
            auto pos_of_FS   = line.find_first_of(FS, begin_index);

            while (pos_of_FS != std::string::npos) {
                const auto part = line.substr(begin_index, pos_of_FS - begin_index);
                const auto p    = parse_number(part);
                if (p.has_value()) current_fields.push_back(p.value());
                else
                    current_fields.push_back(part);

                begin_index = pos_of_FS + 1;
                pos_of_FS   = line.find_first_of(FS, begin_index);
            }
            // At this point pos_of_FS is npos
            // but begin_index (b) has two possibilites depending if line ends in FS (' ') or not
            // "A B C"      "A B C "
            //      b              b
            // In the first case last field is still not handled

            if (begin_index < line.size()) {
                const auto part = line.substr(begin_index);
                const auto p    = parse_number(part);
                if (p.has_value()) current_fields.push_back(p.value());
                else
                    current_fields.push_back(part);
            }
        };

        auto printer =
            overloaded{ [&](const double d) { output << d; },
                        [&](const field f) {
                            std::visit(var_t_printer, get_field(f.n).value_or(""s));
                        },
                        [&](const var_index v) { std::visit(var_t_printer, get_variable(v.i)); },
                        [&](const str_data s) { output << std::string_view{ s }; } };

        auto execute_print_n = [&](const std::span<byte_code const> args) -> void {
            auto assert_to_be_data = [](const byte_code c) {
                if (not std::holds_alternative<data>(c))
                    throw std::logic_error{ "Can not print non-data!" };
            };

            if (args.size() == 0) return;
            if (args.size() == 1) {
                assert_to_be_data(args.front());
                std::visit(printer, std::get<data>(args.front()));
                return;
            }

            const auto size_minus_one = args.size() - 1;
            // Circumvention of gcc 14.0.1 20240115 false positive -fdangling-reference
            const auto circumvention = args | std::views::take(size_minus_one);
            for (const auto x : circumvention) {
                assert_to_be_data(x);
                std::visit(printer, std::get<data>(x));
                // Index 1 is OFS
                std::visit(var_t_printer, get_variable(1));
            }
            assert_to_be_data(args.back());
            std::visit(printer, std::get<data>(args.back()));
            output << "\n";
        };

        auto execute_copy = [&](const byte_code from, const byte_code to) {
            if (not std::holds_alternative<data>(to))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_data = std::get<data>(to);

            if (not std::holds_alternative<var_index>(to_data))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_index = std::get<var_index>(to_data).i;

            if (not std::holds_alternative<data>(from))
                throw std::logic_error{ "Can not copy from non-data!" };

            const auto from_data = std::get<data>(from);
            set_variable_from_data(to_index, from_data);
        };

        auto get_var_t = overloaded{ [&](const double d) { return var_t{ d }; },
                                     [&](const field f) { return get_field(f.n).value_or(""s); },
                                     [&](const var_index v) { return get_variable(v.i); },
                                     // Ugliest of ugly
                                     [&](const str_data s) {
                                         return var_t{ std::string{ std::string_view{ s } } };
                                     } };

        /// And if not it has to be string
        auto add_data = [&](const data lhs, const data rhs) -> var_t {
            auto lhs_var_t = std::visit(get_var_t, lhs);
            auto rhs_var_t = std::visit(get_var_t, rhs);

            // Normally can not add string to double but if either is empty string
            // and the other is number then treat the empty string as 0.0.

            if (std::holds_alternative<double>(lhs_var_t)
                and std::holds_alternative<std::string>(rhs_var_t)) {
                if (std::get<std::string>(rhs_var_t).empty()) {
                    rhs_var_t = var_t{ 0.0 };
                    goto add_numbers;
                }

                throw std::logic_error{ "Can not add number to non-empty string!" };
            } else if (std::holds_alternative<std::string>(lhs_var_t)
                       and std::holds_alternative<double>(rhs_var_t)) {
                if (std::get<std::string>(lhs_var_t).empty()) {
                    lhs_var_t = var_t{ 0.0 };
                    goto add_numbers;
                }

                throw std::logic_error{ "Can not add non-empty string to double!" };
            }

            // lhs and rhs has to be the same type
            if (std::holds_alternative<std::string>(lhs_var_t)) {
                return var_t{ std::get<std::string>(lhs_var_t) + std::get<std::string>(rhs_var_t) };
            } else {
            add_numbers:
                return var_t{ std::get<double>(lhs_var_t) + std::get<double>(rhs_var_t) };
            }
        };

        auto execute_add = [&](const byte_code lhs, const byte_code rhs, const byte_code to) {
            if (not std::holds_alternative<data>(lhs))
                throw std::logic_error{ "Can not add non-data!" };
            if (not std::holds_alternative<data>(rhs))
                throw std::logic_error{ "Can not add non-data!" };

            if (not std::holds_alternative<data>(to))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_data = std::get<data>(to);

            if (not std::holds_alternative<var_index>(to_data))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_index = std::get<var_index>(to_data).i;

            set_variable(to_index, add_data(std::get<data>(lhs), std::get<data>(rhs)));
        };

        auto execute_sub = [&](const byte_code lhs, const byte_code rhs, const byte_code to) {
            if (not std::holds_alternative<data>(lhs))
                throw std::logic_error{ "Can not subtract non-number!" };
            if (not std::holds_alternative<data>(rhs))
                throw std::logic_error{ "Can not subtract non-number!" };

            auto lhs_var_t = std::visit(get_var_t, std::get<data>(lhs));
            auto rhs_var_t = std::visit(get_var_t, std::get<data>(rhs));

            // Normally can not subtract string from double or other way around
            // but if either is empty string and the other is number then treat the empty string as 0.0

            const auto lhs_is_str = std::holds_alternative<std::string>(lhs_var_t);
            const auto rhs_is_str = std::holds_alternative<std::string>(rhs_var_t);

            if (lhs_is_str and rhs_is_str) {
                throw std::logic_error{ "Can not subtract string from string!" };
            } else if (lhs_is_str and not rhs_is_str) {
                if (std::get<std::string>(lhs_var_t).empty()) {
                    lhs_var_t = var_t{ 0.0 };
                    goto way_out;
                }

                throw std::logic_error{ "Can not subtract from non-empty string!" };
            } else if (not lhs_is_str and rhs_is_str) {
                if (std::get<std::string>(rhs_var_t).empty()) {
                    rhs_var_t = var_t{ 0.0 };
                    goto way_out;
                }

                throw std::logic_error{ "Can not subtract non-empty string!" };
            }

        way_out:
            const auto lhs_d = std::get<double>(lhs_var_t);
            const auto rhs_d = std::get<double>(rhs_var_t);

            if (not std::holds_alternative<data>(to))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_data = std::get<data>(to);

            if (not std::holds_alternative<var_index>(to_data))
                throw std::logic_error{ "Can not copy to non-variable!" };

            const auto to_index = std::get<var_index>(to_data).i;

            set_variable(to_index, var_t{ lhs_d - rhs_d });
        };

        auto check_that_arguments_exits = [&](const std::size_t index_of_instruction,
                                              const std::size_t how_many_should_be,
                                              const std::size_t total_amount_of_byte_codes) {
            if (index_of_instruction + how_many_should_be >= total_amount_of_byte_codes)
                throw std::logic_error{ "Not enough arguments to execute!" };
        };

        auto execute_bytes = [&](const std::span<byte_code const> bytes) -> void {
            for (auto i = 0uz; i < bytes.size(); ++i) {
                if (std::holds_alternative<null>(bytes[i])) {
                    return;
                } else if (std::holds_alternative<print_n>(bytes[i])) {
                    const auto n = std::get<print_n>(bytes[i]).n;
                    check_that_arguments_exits(i, n, bytes.size());
                    execute_print_n(std::span<byte_code const>{ &bytes[i + 1], n });
                    i += n;
                } else if (std::holds_alternative<copy>(bytes[i])) {
                    check_that_arguments_exits(i, 2, bytes.size());
                    execute_copy(bytes[i + 1], bytes[i + 2]);
                    i += 2;
                } else if (std::holds_alternative<add>(bytes[i])) {
                    check_that_arguments_exits(i, 3, bytes.size());
                    execute_add(bytes[i + 1], bytes[i + 2], bytes[i + 3]);
                    i += 3;
                } else if (std::holds_alternative<sub>(bytes[i])) {
                    check_that_arguments_exits(i, 3, bytes.size());
                    execute_sub(bytes[i + 1], bytes[i + 2], bytes[i + 3]);
                    i += 3;
                } else {
                    throw std::logic_error{ "Executing data :/" };
                }
            }
        };

        execute_bytes(awk.begin_code_);
        for (const auto line : str | std::views::split('\n')) {
            set_fields(std::string{ line.begin(), line.end() });
            execute_bytes(awk.command_code_);
        }
        execute_bytes(awk.end_code_);

        return output.str();
    }
};

template<fixed_string<100> expr>
constexpr auto operator""_awk() -> awk_engine {
    return { expr.sv() };
};

} // namespace awk
