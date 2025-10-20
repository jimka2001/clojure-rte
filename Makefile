statistics: deps
	clj -M -m statistics-rte 10 25 25

classic:
	clj -M -m statistics-rte classic 50

partially-balanced:
	clj -M -m statistics-rte partially-balanced 20

totally-balanced:
	clj -M -m statistics-rte totally-balanced 100

deps:
	clj -Sdeps '{:deps {io.github.borkdude/lein2deps {:mvn/version "0.1.1"}}}' \
		-M -m lein2deps.api --print --write-file deps.edn

simple: deps
	clj -M -m demos.conj-2025.cli tree-split-rte-gaussian 1
	clj -M -m demos.conj-2025.cli tree-split-rte-inv-gaussian 1
	clj -M -m demos.conj-2025.cli tree-split-rte-linear 1
	clj -M -m demos.conj-2025.cli comb-rte 1
	clj -M -m demos.conj-2025.cli flajolet 1

loop:
	for i in $$(seq 1 10); do \
		clj -M -m clojureconj-cli tree-split-rte-gaussian 1 ; \
		clj -M -m clojureconj-cli tree-split-rte-inv-gaussian 1 ; \
		clj -M -m clojureconj-cli tree-split-rte-linear 1 ; \
		clj -M -m clojureconj-cli comb-rte 1 ; \
		clj -M -m clojureconj-cli flajolet 1 ; \
	done

