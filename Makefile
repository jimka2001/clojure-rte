statistics:
	clj -Sdeps '{:deps {io.github.borkdude/lein2deps {:mvn/version "0.1.1"}}}' -M -m lein2deps.api --print --write-file deps.edn
	clj -M -m statistics 10 25 25
