SOURCES = \
GeneticAlgorithm.ml \
Guess.ml \
Polygon.ml \
Main.ml \

all: $(SOURCES)
	corebuild -lib graphics -pkg camlimages.all_formats Main.native

clean:
	rm -rf _build Main.native
