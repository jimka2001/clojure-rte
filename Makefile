statistics: deps
	clj -M -m statistics-rte 10 25 25

classic:
	clj -M -m statistics-rte classic 50

partially-balanced:
	clj -M -m statistics-rte partially-balanced 20

totally-balanced:
	clj -M -m statistics-rte totally-balanced 100

deps:
	clj -Sdeps '{:deps {io.github.borkdude/lein2deps {:mvn/version "0.1.1"}}}' -M -m lein2deps.api --print --write-file deps.edn
