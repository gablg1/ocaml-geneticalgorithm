SOURCES = \
GeneticAlgorithm.ml \
Guess.ml \
Polygon.ml \
Main.ml \

all: $(SOURCES)
	corebuild -lib graphics Main.native

clean:
	rm -rf _build Main.native
