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
	clj -M -m demos.conj-2025.cli tree-split-gauss 1
	clj -M -m demos.conj-2025.cli tree-split-inv-gauss 1
	clj -M -m demos.conj-2025.cli tree-split-linear 1
	clj -M -m demos.conj-2025.cli comb 1
	clj -M -m demos.conj-2025.cli flajolet 1

test: deps
	clj -M -m demos.conj-2025.cli tree-split-gauss 1


loop:
	for i in $$(seq 1 10); do \
		clj -M -m demos.conj-2025.cli tree-split-gauss 10 ; \
		clj -M -m demos.conj-2025.cli tree-split-inv-gauss 10 ; \
		clj -M -m demos.conj-2025.cli tree-split-linear 10 ; \
		clj -M -m demos.conj-2025.cli comb 10 ; \
		clj -M -m demos.conj-2025.cli flajolet 10 ; \
	done

