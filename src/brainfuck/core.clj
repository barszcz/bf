(ns brainfuck.core
  (:gen-class))

;; This is an interpreter for the esoteric programming language "brainfuck",
;; Written in Clojure.
;; (Hey, it's their name, not mine!)
;; https://en.wikipedia.org/wiki/Brainfuck
;; Featuring:
;; - A handwritten *purely functional* parser
;; - An interpreter, which is also purely functional

(declare parse-token)
(declare parse-loop)
(declare run-exprs)

;; PARSER
;; Wasn't sure if using a parser generator library counted as a "framework"
;; So I wrote a recursive descent parser by hand.
;; At least, I think it's recursive descent? I don't know enough about parsers to be sure.
;; It works, though.
;; Since all tokens in brainfuck are one character,
;; it didn't seem necessary to write a separate lexer/tokenizer.

(defn parse-tokens
  "Generates a brainfuck abstract syntax tree from source code."
  [source]
  (loop [ast []
         tokens source]
    (let [[new-ast rest-tokens] (parse-token ast tokens)]
      (if (seq rest-tokens)
        (recur new-ast rest-tokens)
        new-ast))))

(defn- parse-token [exprs [token & tokens]]
  "Takes the accumulated vector of AST expressions and the tokens yet to be parsed.
  Switches on the first token and then adds the corresponding expression
  to the expressions vector, returning that and the tokens yet to be consumed.
  "
  (let [add-expr #(conj exprs %)]
    (case token
      \+ [(add-expr {:type :inc-value}) tokens]
      \- [(add-expr {:type :dec-value}) tokens]
      \> [(add-expr {:type :inc-pointer}) tokens]
      \< [(add-expr {:type :dec-pointer}) tokens]
      \. [(add-expr {:type :output}) tokens]
      \, [(add-expr {:type :input}) tokens]
      ; loop logic is more complicated so I pulled it into its own func
      \[ (let [[loop-expr remaining-tokens] (parse-loop tokens)]
           [(add-expr loop-expr) remaining-tokens])
      ; `parse-loop` looks for/consumes ] so we should never see one here
      \] (throw (Exception. (str "unbalanced loop")))
      ; In case of an unrecognized 'token' (character), return the exprs vector unchanged
      [exprs tokens])))

; Loops were the hard part.
(defn- parse-loop
  "Receives the remaining tokens after an opening bracket and calls `parse-token`
  successively on them, accumulating the results. When a closing bracket is
  reached, returns an expression with the accumulated subexpressions vector
  and the remaining tokens.
  Since `parse` calls `parse-loop`, if a program contains a nested loop it
  will recurse as many levels deep as it needs to. Just don't feed it anything
  that'll blow the JVM stack.
  A potential refactor here would be to rewrite this in terms of `parse-tokens`.
  "
  ([tokens] (parse-loop [] tokens))
  ([exprs tokens]
   (case (first tokens)
     nil (throw (Exception. (str "unbalanced loop")))
     \] [{:type :loop :exprs exprs} (rest tokens)]
     (let [[new-exprs new-tokens] (parse-token exprs tokens)]
       (recur new-exprs new-tokens)))))


;; INTERPRETER
;; `run-ast` takes the AST generated from `parse-source` and... runs it.
;; Like the parser this is purely functional and threads the program state
;; explicitly through various function calls.

; These are just helper funcs to read/write the currently pointed-to
; value in memory.
(defn- get-current-val [state]
  (let [loc (:pointer state)]
    (get-in state [:memory loc])))

(defn- update-current-val [state f]
  (let [loc (:pointer state)]
    (update-in state [:memory loc] f)))

(defn- assoc-current-val [state new-val]
  (let [loc (:pointer state)]
    (assoc-in state [:memory loc] new-val)))


(defmulti
  run
  "Multimethod that dispatches on the 'type' of an expression,
  performing the corresponding operation on the passed program state.
  Takes an expression and state and returns a new state."
  (fn [expr _] (:type expr)))


(defmethod run :inc-value [_ state]
  (update-current-val state inc))

(defmethod run :dec-value [_ state]
  (update-current-val state dec))

(defmethod run :inc-pointer [_ state]
  (when (= (count (:memory state)) (:pointer state))
    (throw (Exception. (str "pointer out of bounds"))))
  (update state :pointer inc))

(defmethod run :dec-pointer [_ state]
  (when (zero? (:pointer state))
    (throw (Exception. (str "pointer out of bounds"))))
  (update state :pointer dec))

(defmethod run :output [_ state]
  (-> state
      get-current-val
      char
      print)
  state)

(defmethod run :input [_ state]
  (let [loc (:pointer state)]
    (->> (read-line)
         first ; we only care about the first char of input
         int
         (assoc-current-val state))))

(defmethod run :loop
  [{sub-exprs :exprs :as expr} state]
  (let [val (get-current-val state)]
    (if (zero? val)
      state
      (let [new-state (run-exprs sub-exprs state)]
        (recur expr new-state)))))


(defn- run-exprs
  "Executes a vector of expressions in sequence, threading the state through."
  [[expr & exprs] state]
  (if (nil? expr)
    state
    (recur exprs (run expr state))))


(defn run-ast
  "Takes an AST (generated from `parse-tokens`) and runs it with a newly initialized program state."
  ([ast]
   (run-ast ast 30000))
  ([ast memory-size]
   (let [initial-memory (->> 0
                             (repeat memory-size)
                             (into []))
         initial-pointer 0
         initial-state {:memory  initial-memory
                        :pointer initial-pointer}]
     (run-exprs ast initial-state)
     (println)
     nil)))

(defn run-program [source]
  (-> source
      parse-tokens
      run-ast))


;; Here are a couple sample programs to run.
(def hello-world "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
(def echo ",.")
(def jabh "+++[>+++++<-]>[>+>+++>+>++>+++++>++<[++<]>---]>->-.[>++>+<<--]>--.--.+.>>>++.<<.<------.+.+++++.>>-.<++++.<--.>>>.<<---.<.-->-.>+.[+++++.---<]>>[.--->]<<.<+.++.++>+++[.<][http://www.hevanet.com/cristofd/brainfuck/]<.")
(def quine "->++>+++>+>+>+++>>>>>>>>>>>>>>>>>>>>+>+>++>+++>++>>+++>+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+>+>>+++>>+++>>>>>+++>+>>>>>>>>>++>+++>+++>+>>+++>>>+++>+>++>+++>>>+>+>++>+++>+>+>>+++>>>>>>>+>+>>>+>+>++>+++>+++>+>>+++>>>+++>+>++>+++>++>>+>+>++>+++>+>+>>+++>>>>>+++>+>>>>>++>+++>+++>+>>+++>>>+++>+>+++>+>>+++>>+++>>++[[>>+[>]++>++[<]<-]>+[>]<+<+++[<]<+]>+[>]++++>++[[<++++++++++++++++>-]<+++++++++.<]")
;; this ROT13 accepts input until it gets a blank input, at which point it prints and terminates
(def rot13 ",[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>>+++++[<----->-]<<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>>+++++[<----->-]<<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]")

(defn -main
  "concatenates arguments from stdin and runs them as a brainfuck program."
  [& args]
  (run-program (apply str args)))
