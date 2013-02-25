.PHONY: all clean cases

all: report.html report.csv

clean:
	rm -f Bench Bench.hi Bench.o

report.csv report.html: Bench $(wildcard cases/*)
	./Bench +RTS -K33554432 -RTS -g -o report.html -u report.csv -t templates/report.tpl $(wildcard cases/*)

Bench: Bench.hs
	ghc -i../dist/build/autogen -rtsopts --make Bench.hs 
