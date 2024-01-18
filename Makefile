showcases=NF numbers pipe str

all: $(showcases)

% : showcase_%.cpp
	$(CXX) -std=c++23 -Wall -Werror $< -o $@

.phony: clean

clean:
	rm $(showcases)
