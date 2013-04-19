anagrams: Anagrams.hs Main.hs
	ghc --make Main -o anagrams

debug: Anagrams.hs Main.hs
	ghc --make Main -o anagrams -prof -auto-all


all: anagrams

