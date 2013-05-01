anagrams: Anagrams.hs Main.hs
	ghc --make Main -o anagrams

llvm: Anagrams.hs Main.hs
	ghc --make Main -fllvm -o anagrams

prof: Anagrams.hs Main.hs
	ghc --make Main -o anagrams -prof -auto-all

clean:
	rm -f anagrams

all: anagrams

