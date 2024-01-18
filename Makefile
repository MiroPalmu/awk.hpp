showcases=NF numbers pipe str

all: $(showcases) benchmark

% : showcase_%.cpp
	$(CXX) -std=c++23 -Wall -Werror $< -o $@

benchmark : benchmark.cpp
	$(CXX) -std=c++23 -Wall -Werror -O3 -march=native $< -o $@

.phony: clean

clean:
	rm -f $(showcases) benchmark
