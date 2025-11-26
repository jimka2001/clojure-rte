statistics: deps
	clj -M -m statistics-rte 10 25 25

classic:
	clj -M -m statistics-rte classic 50

partially-balanced:
	clj -M -m statistics-rte partially-balanced 20

totally-balanced:
	clj -M -m statistics-rte totally-balanced 100

tbnl:
	clj -M -m statistics-rte tbnl 100

deps:
	clj -Sdeps '{:deps {io.github.borkdude/lein2deps {:mvn/version "0.1.1"}}}' \
		-M -m lein2deps.api --print --write-file deps.edn

simple: deps
	clj -M -m demos.conj-2025.cli tree-split-gauss 1
	clj -M -m demos.conj-2025.cli tree-split-inv-gauss 1
	clj -M -m demos.conj-2025.cli tree-split-linear 1
	clj -M -m demos.conj-2025.cli comb 1
	clj -M -m demos.conj-2025.cli flajolet 1

test-tbnl:
	clj -M -m demos.conj-2025.cli tbnl 50


tests:
	lein test

test-genus-spec:
	lein test :only genus-spec-test

test-xymbolyco:
	lein test :only xymbolyco-test

test-genus:
	lein test :only genus-test genus-conversion-test genus-disjoint-test genus-equiv-test genus-spec-test genus-statistics-test genus-subtype-test

loop:
	for i in $$(seq 1 10); do \
		clj -M -m demos.conj-2025.cli tree-split-gauss 10 ; \
		clj -M -m demos.conj-2025.cli tree-split-inv-gauss 10 ; \
		clj -M -m demos.conj-2025.cli tree-split-linear 10 ; \
		clj -M -m demos.conj-2025.cli comb 10 ; \
		clj -M -m demos.conj-2025.cli flajolet 10 ; \
	done

