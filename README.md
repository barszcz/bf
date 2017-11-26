# Brainfuck

A [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter written in Clojure.

Most of the documentation is in the code itself (src/brainfuck/core.clj).

To run (NB: You must have [leiningen](https://leiningen.org/) installed on your machine):
`git clone git@github.com:barszcz/brainfuck.git`
`cd brainfuck`
`lein uberjar`
`java -jar target/uberjar/brainfuck-0.1.0-SNAPSHOT-standalone.jar "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."`
This should print "Hello World!" to the screen.

Have fun!