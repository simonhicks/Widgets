(ns widgets.js-gen
  (:use [clojure.contrib.core :only (seqable?)])
  (:use [clojure.walk :only (walk prewalk)])
  (:use [clojure.contrib.str-utils :only (re-split)])
  (:use [clojure.java.io :only (as-file as-relative-path)])
  (:require [clojure.string :as s]))

;;;;;;;;; Road map
; - try/catch/finally (try try-expr catch-fn? finally-expr?)
; - let (local binding created by anonymous function call)
;   - destructuring ???
; - js-doc 
;   - function for looking up documentations of js-forms
;   - can't use meta-data because many js special forms aren't defined
; - refactor to make everything more readable and more encapsulated.
; - type-hints and useful compiler-errors/warnings

(defmacro deflots 
  [macro-name & coll]
  `(do ~@(map (fn [a#] `(~macro-name ~a#)) coll)))

(defmulti emit (fn [context expr] [context (class expr)]))

(derive ::statement  ::not-return)
(derive ::inline     ::not-return)
(derive ::not-return ::any-context)
(derive ::return     ::any-context)

(defmethod emit :default [_ expr]
  (str expr))

(defmethod emit [::return java.lang.Object] [_ expr]
  (str "return " (emit ::inline expr)))

(defmethod emit [::not-return nil] [_ _] "null")

(defmethod emit [::not-return java.lang.String] [_ expr]
  (str \" (.. expr (replace "\n" "\\n") (replace "\"" "\\\"") (replace "\t" "\\t") (replace "\r" "\\r")) \"))

(defmethod emit [::not-return clojure.lang.Ratio] [_ expr]
  (str (float expr)))

(defn valid-js-symbol? [sym]
  (boolean (re-matches #"^[a-zA-Z_$][0-9a-zA-Z_$.]*$" sym)))

(derive clojure.lang.Symbol ::js-symbol)
(derive clojure.lang.Keyword ::js-symbol)

(def js-reserved
  #{:break :continue :do :for :import :new :this :void :case :default :else 
    :function :in :return :typeof :while :comment :delete :export :if :labal 
    :switch :var :with :abstract :boolean :byte :char :double :false :final 
    :goto :implements :instanceOf :int :interface :long :native :null :package 
    :private :protected :public :short :static :synchronized :throws 
    :transient :true :catch :class :const :debugger :enum :extends :finally 
    :super :throw :try})

(defmethod emit [::not-return ::js-symbol] [_ expr]
  (let [sym (name expr)]
    (when-not (valid-js-symbol? sym) (throw (Error. (str sym " is not a valid javascript symbol"))))
    sym))

(defmethod emit [::not-return java.util.regex.Pattern] [_ expr]
  (str \/ expr \/))

(defmethod emit [::not-return clojure.lang.IPersistentVector] [_ expr]
  (str "[" (s/join ", " (map #(emit ::inline %)  expr)) "]"))

(defmethod emit [::not-return clojure.lang.LazySeq] [context expr]
  (emit context (into [] expr)))

(defmethod emit [::not-return clojure.lang.IPersistentMap] [_ expr]
  (let [escape #(if (js-reserved %) (str (name %)) %)]
    (str "{\n" 
         (s/join ",\n" (for [[k v] expr] (str (emit ::inline (escape k)) ": " (emit ::inline v)))) 
         "\n}")))

(derive clojure.lang.Cons ::list)
(derive clojure.lang.IPersistentList ::list)

(defn arg-list [args]
  (str "(" (s/join ", " (map #(emit ::inline %) args)) ")"))

(defn function-call [context expr]
  (str (emit context (first expr)) (arg-list (rest expr))))

(defn method-call? [form]
  (and
    (symbol? form)
    (= \. (first (str form)))))

(defn method-call
  "handles the shorthand form of method calls (.method obj args)"
  [context expr]
  (let [[method-sym obj & args] expr
        method                  (symbol (subs (str method-sym) 1))
        d-sym                   '.]
    (emit context `(~d-sym ~obj ~method ~@args))))

(defn constructor? [form]
  (and
    (symbol? form)
    (= \. (last (str form)))))

(defn constructor
  "handles the shorthand form for object construction (Foo. args) => new Foo(args)"
  [context expr]
  (let [[class-sym & args] expr
        klass              (symbol (apply str (drop-last (str class-sym))))
        n-sym              'new]
    (emit context `(~n-sym ~klass ~@args))))

(declare emit-special special-form?)

(defmethod emit [::any-context ::list] [context expr]
  (let [f (first expr)]
    (cond
      (special-form? f) (emit-special  context expr)
      (constructor?  f) (constructor   context expr)
      (method-call?  f) (method-call   context expr)
      :else             (function-call context expr))))
(prefer-method emit [::any-context ::list] [::return java.lang.Object])

(defmulti emit-special (fn [context expr] [context (first expr)]))

(def special-forms (ref #{}))

(defn special-form? [expr] (@special-forms expr))

(defmacro defspecial [sym context & forms]
  (do
    (dosync (alter special-forms conj sym))
    `(defmethod emit-special [~context (quote ~sym)] ~@forms)))

(defmacro bracketed-inline
  [sym]
  `(defspecial ~sym ::inline
     [_# expr#]
     (str "(" (emit ::statement expr#) ")")))

(defmacro standard-return
  [sym]
  `(defspecial ~sym ::return
      [_# expr#]
      (str "return " (emit ::inline expr#))))

(defmacro definfix [sym]
   `(do
      (defspecial ~sym ::statement
        [_# expr#]
        (let [args# (next expr#)]
          (s/join ~(str " " (name sym) " ") (map (fn [a#] (emit ::inline a#)) args#))))
      (bracketed-inline ~sym)
      (standard-return ~sym)))
(deflots definfix + - * / % & | && ||)

(defmacro defcomparator [sym]
  (let [sym-str (str " " (name sym) " ")]
    `(do
       (defspecial ~sym ::statement
          [_# expr#]
          (let [args# (map (fn [x#] (emit ::inline x#)) (next expr#))]
            (str "(" (s/join ") && (" (map (fn [a#] (s/join ~sym-str a#)) (partition 2 1 args#))) ")")))
       (bracketed-inline ~sym)
       (standard-return ~sym))))
(deflots defcomparator >= <= > <)

(defspecial = ::statement
  [_ expr]
  (let [args (map (fn [e] (emit ::inline e)) (next expr))]
    (str "(" (s/join ") && (" (map #(s/join " === " %) (partition 2 1 args))) ")")))

(defspecial != ::statement
  [_ expr]
  (let [args (map (fn [e] (emit ::inline e)) (next expr))]
    (str "(" (s/join ") && (" (map #(s/join " !== " %) (partition 2 1 args))) ")")))

(defspecial ! ::statement
  [_ expr]
  (let [args (second expr)
        bool (emit ::inline args)]
    (str "!" bool)))

(defspecial not ::not-return
  [_ expr]
  (let [args (second expr)
        bool (emit ::inline args)]
    (str "!" bool)))

;(deflots bracketed-inline = != ! not)

; special forms for method calls
;   (. obj method)          => "obj.method()"
;   (.. obj attr (method))  => "obj.attr.method()"

(defspecial . ::not-return
  [_ expr]
  (let [[obj & more] (next expr)]
    (str (emit ::inline obj) \. (emit ::inline more))))

(defspecial .. ::not-return
  [_ expr]
  (let [[obj & more] (next expr)]
    (str (emit ::inline obj) \. (s/join \. (map #(emit ::inline %) more)))))

;(deflots standard-return = != ! not . ..)

(defspecial do ::statement
  [_ expr]
  (str (s/join ";\n" (map #(emit ::statement %) (rest expr))) ";\n"))

(defspecial do ::inline
  [_ expr]
  (let [f-sym 'fn]
    (emit ::inline `((~f-sym [] ~@(rest expr))))))

(defspecial do ::return
  [_ expr]
  (let [args (next expr)]
    (if (> (count args) 1)
        (let [return-expr  (last args)
              side-expr    (drop-last args)]
          (str
             (when-not (empty? side-expr) (str (s/join ";\n" (map #(emit ::statement %) side-expr)) ";\n"))
             (str (emit ::return return-expr) ";\n")))
        (emit ::return (last expr)))))

(declare vars)

(defn var-declarations
  [] (when-not (empty? @vars) (str "var " (s/join ", " (map #(emit ::inline %) @vars)))))

(defspecial set! ::statement
  [_ expr]
  (let [[n v] (rest expr)]
    (str (emit ::inline n) " = " (emit ::inline v))))

(defspecial var ::statement
  [context expr]
  (let [[_ n v] expr]
    (if (bound? (var vars))
      (dosync 
        (alter vars conj (second expr))
        (if v (emit context `(set! ~n ~v))))
      (str "var " (emit ::inline n) " = " (emit ::inline v)))))

(defn emit-local-scope
  [context & exprs]
  (binding [vars (ref #{})]
    (let [body (emit context `(do ~@exprs))]
      (str (var-declarations) ";\n" body))))

(defspecial aget ::not-return
  [_ expr]
  (let [[ary idx] (next expr)]
    (str (emit ::inline ary) "[" (emit ::inline idx) "]")))

(defspecial aset ::statement
  [_ expr]
  (let [[ary idx value] (next expr)]
    (str (emit ::inline ary) "[" (emit ::inline idx) "] = " (emit ::inline value))))

(defspecial exists? ::statement
  [_ expr]
  (let [thing (emit ::inline (second expr))]
    (str "typeof " thing " != \"undefined\" && " thing " !== null")))

(defspecial ?= ::any-context
  [context expr]
  (let [[_ x y] expr]
    (emit context (list 'if (list 'exists? x) x (list 'var x y)))))

(defspecial inc! ::statement
  [_ expr]
  (let [[_ v n] expr
        number  (or n 1)]
    (str (emit ::inline v) " += " (emit ::inline number))))

(defspecial inc ::statement
  [_ expr]
  (let [[_ v n] expr
        number (or n 1)]
    (str (emit ::inline v) " + " (emit ::inline number))))

(defspecial dec! ::statement
  [_ expr]
  (let [[_ v n] expr
        number  (or n 1)]
    (str (emit ::inline v) " -= " (emit ::inline number))))

(defspecial dec ::statement
  [_ expr]
  (let [[_ v n] expr
        number (or n 1)]
    (str (emit ::inline v) " - " (emit ::inline number))))

;(deflots bracketed-inline aset aget exists? set! var inc! dec! inc dec)
;(deflots standard-return aset aget exists? set! var inc! dec! inc dec)

(defspecial ? ::not-return
  [_ expr]
  (let [[_ test-expr then-expr else-expr] expr]
    (str (emit ::inline test-expr) " ? " (emit ::inline then-expr) " : " (emit ::inline else-expr))))
;(standard-return ?)

(defspecial if ::statement
  [_ expr]
  (let [[test-form then else] (rest expr)]
    (str "if " (emit ::inline test-form) " {\n" 
         (emit-local-scope ::statement then) "\n}" 
         (when else (str " else {\n" (emit-local-scope ::statement else) "\n}")))))

(defspecial unless ::not-return
  [context expr]
  (let [[test-form then else] (rest expr)]
    (emit context (list 'if (list 'not test-form) then else))))

(defspecial ||= ::any-context
  [context expr]
  (let [[_ x y] expr]
    (emit context (list '|| x (list 'set! x y)))))

(defn emit-function-body
  [exprs]
  (apply (partial emit-local-scope ::return) exprs))

(defn parse-arg-list
  [args]
  (let [[seps [_ whole]]  (split-with #(not (= :as %)) args)
        [reg [_ splat]]   (split-with #(not (= '& %)) seps)]
    [reg splat whole]))

(defn fn-arg-decs
  "returns js s-exprs for the arg declarations [regs & splat :as whole]"
  [regs splat whole]
  (let [[arg-count inf]   (vector (count regs) (boolean splat))
        indexed-regs      (map #(vector %1 %2) (iterate inc 0) regs)
        reg-decs          (for [[idx r] indexed-regs] (list 'var  r (list 'aget 'arguments idx)))
        whole-dec         (if whole (list 'var whole (list '.. 'Array 'prototype 'slice (list 'call 'arguments 0))))
        splat-dec         (if splat (list 'var splat (list '? (list '>= 'arguments.length (+ arg-count 1)) 
                                                              (list '.. 'Array 'prototype 'slice (list 'call 'arguments arg-count))
                                                              [])))]
    (filter #(not (nil? %)) `(~splat-dec ~whole-dec))))

(defn emit-simple-function
  [args exprs]
  (let [[regs splat whole] (parse-arg-list args)
        body-exprs         `(~@(fn-arg-decs regs splat whole) ~@exprs)]
    (str "function " (arg-list regs) " {\n" (emit-function-body body-exprs) "\n}")))

(defn overloaded-fn-cond-pair
  "builds a test-expr/then pair for use in an overloaded function's cond expr"
  [func args]
  (let [[regs splat _] (parse-arg-list args)
        comp-fn        (if splat '> '=)
        n              (count regs)]
    (list (list comp-fn 'arguments.length n) (list '.apply func 'this 'arguments))))

(defn overloaded-function
  [exprs]
  (let [arg-vectors   (map first exprs)
        args          (apply vector (last (sort-by count (map (comp first parse-arg-list) arg-vectors))))
        make-name     #(symbol (str "__f" %))
        index-fn      #(vector (make-name %1) (first %2) (rest %2)) ; collect the dummy function name, arg vector and body in a vector
        indexed-fns   (map #(index-fn %1 %2) (iterate inc 0) exprs)
        fn-decs       (map (fn [a] (list 'var (a 0) `(~(symbol "fn") ~(a 1) ~@(a 2)))) indexed-fns)
        cond-expr     `(~(symbol "cond") ~@(apply concat (map #(overloaded-fn-cond-pair (% 0) (% 1)) indexed-fns)))]
    `(~(symbol "fn") ~args ~@fn-decs ~cond-expr)))

(defspecial fn ::any-context
  [_ expr]
  (let [args (next expr)
        overloaded? #(not (= clojure.lang.PersistentVector (class (first %))))]
    (if (overloaded? args)
        (emit ::statement (overloaded-function args))
        (emit-simple-function (first args) (next args)))))

(defspecial if ::return
  [_ expr]
  (let [[test-form then else] (rest expr)]
    (str "if " (emit ::inline test-form) " {\n"
         (emit-function-body (list then)) "\n}"
         (when else (str " else {\n" (emit-function-body (list else)) "\n}")))))

(defmacro funcall-inline
  [sym]
  (let [expr (gensym "expr")
        f    'fn]
    `(defspecial ~sym ::inline
       [_# ~expr]
       (str "(" (emit ::statement (list (quote ~f) [] ~expr)) "())"))))

(defspecial cond ::any-context
  [context expr]
  (let [pairs     (partition 2 (rest expr))
        else-func (cond 
                    (= context ::return)    emit-function-body
                    (= context ::statement) (partial emit-local-scope ::statement))]
      (s/join " else " (for [[test-form then-form] pairs] 
                         (if (= (class test-form) clojure.lang.Keyword)
                             (str "{\n" (emit-function-body then-form) "\n}")
                             (emit context `(if ~test-form ~then-form)))))))
(prefer-method emit-special [::inline 'cond] [::any-context 'cond])

(defspecial when ::any-context
  [context expr]
  (let [[_ test-form & body] expr
        i-sym                'if]
    (emit context `(~i-sym ~test-form (do ~@body)))))

(defspecial when-not ::any-context
  [context expr]
  (let [[_ test-form & body] expr
        u-sym                'unless]
    (emit context `(~u-sym ~test-form (do ~@body)))))

(defspecial while ::statement
  [_ expr]
  (let [[_ test-form & body] expr]
    (str "while" (emit ::inline test-form) " {\n" (apply (partial emit-local-scope ::statement) body) "\n}")))

(defspecial while ::return
  [_ expr]
  (let [[_ test-form & body] expr]
    (str "while" (emit ::inline test-form) " {\n" (emit-function-body body) "\n}")))

(defspecial until ::any-context
  [context expr]
  (let [[_ test-form & body] expr
        w-sym                'while
        n-sym                'not]
  (emit context `(~w-sym (~n-sym ~test-form) ~@body))))

(defspecial recur ::not-return
  [_ expr]
  (let [[_ & args] expr]
    (str "arguments.callee" (arg-list args))))
;(standard-return recur)

(defspecial new ::statement
  [_ expr]
  (let [[_ obj & args] expr]
    (str "new " (emit ::inline obj) (arg-list args))))

;(bracketed-inline new)
;(standard-return new)

(defspecial defn ::any-context
  [context expr]
  (let [[_ f-name & more] expr
        v-sym             'var
        f-sym             'fn]
    (emit context `(~v-sym ~f-name (~f-sym ~@more)))))

(defspecial for-loop ::statement
  [_ expr]
  (let [[_ var-expr cont-pred inc-expr body-expr] expr]
    (str "for (" (emit ::statement var-expr) "; " (emit ::statement cont-pred) "; " (emit ::statement inc-expr) ") {\n"
         (emit ::statement body-expr)
         ";\n}")))

(defspecial for-loop ::return
  [_ expr]
  (emit-function-body (list expr 'null)))

(deflots standard-return aset aget exists? set! var inc! dec! inc dec new = != ! not . .. ? recur)
(deflots bracketed-inline aset aget exists? set! var inc! dec! inc dec new = != ! not)
(deflots funcall-inline if cond while for-loop)

(defn for-comprehension
  [expr]
  (let [[_ seq-exprs body-expr] expr
        group-func              #(if (keyword? (first %)) ::modifier ::binding)
        {bindings ::binding
         modifiers ::modifier} (group-by group-func (partition 2 seq-exprs))
        get-pred               (fn [k] (second (some #(if (= k (first %)) %) modifiers)))
        if-pred                (get-pred :when)
        while-pred             (get-pred :while)
        arg-vector             (apply vector (for [[i coll] bindings] i))
        inc-data               (for [[i coll] bindings] [(symbol (str \_ i)) i coll])
        for-loop-args          (for [[d _ coll] inc-data] (list 'for-loop (list 'var d 0) (list '< d (list '.. coll 'length)) (list 'inc! d)))
        body-var-decs          (for [[d i coll] inc-data] (list 'var i (list 'aget coll d)))
        break-clause           (if while-pred (list 'when-not while-pred 'break))
        result-accumulator     (if if-pred 
                                   (list 'when if-pred (list '.push '_results `(~(symbol "_result") ~@(seq arg-vector))))
                                   (list '.push '_results `(~(symbol "_result") ~@(seq arg-vector))))
        body                   `(~(symbol "do") ~@body-var-decs ~break-clause ~result-accumulator)
        for-loops              (reduce (fn [body loop-args] `(~@loop-args ~body)) (reverse `(~@for-loop-args ~body)))]
    (list
      (list 'fn [] 
            (list 'var '_results [])
            (list 'var '_result (list 'fn arg-vector body-expr))
            for-loops
            '_results))))

(defspecial for ::any-context
  [context expr]
  (let [expanded (for-comprehension expr)]
    (emit context expanded)))

(defn do-sequence
  [expr]
  (let [[_ seq-exprs body-expr] expr
        group-func              #(if (keyword? (first %)) ::modifier ::binding)
        {bindings ::binding
         modifiers ::modifier} (group-by group-func (partition 2 seq-exprs))
        get-pred               (fn [k] (second (some #(if (= k (first %)) %) modifiers)))
        if-pred                (get-pred :when)
        while-pred             (get-pred :while)
        arg-vector             (apply vector (for [[i coll] bindings] i))
        inc-data               (for [[i coll] bindings] [(symbol (str \_ i)) i coll])
        for-loop-args          (for [[d _ coll] inc-data] (list 'for-loop (list 'var d 0) (list '< d (list '.. coll 'length)) (list 'inc! d)))
        body-var-decs          (for [[d i coll] inc-data] (list 'var i (list 'aget coll d)))
        break-clause           (if while-pred (list 'when-not while-pred 'break))
        function-doer          (if if-pred 
                                   (list 'when if-pred `(~(symbol "_do") ~@(seq arg-vector)))
                                   `(~(symbol "_do") ~@(seq arg-vector)))
        body                   `(~(symbol "do") ~@body-var-decs ~break-clause ~function-doer)
        for-loops              (reduce (fn [body loop-args] `(~@loop-args ~body)) (reverse `(~@for-loop-args ~body)))]
    (list
      (list 'fn [] 
            (list 'var '_do (list 'fn arg-vector body-expr))
            for-loops))))

(defspecial doseq ::any-context
  [context expr]
  (let [expanded (do-sequence expr)]
    (emit context expanded)))

(defspecial onload ::any-context
  [context expr]
  (let [[_ & more] expr
        s-sym      'set!
        f-sym      'fn]
    (emit context `(~s-sym (.. window onload) (~f-sym [] ~@more)))))

(defn remove-blank-lines
  [text]
  (let [lines (re-split #"\n" text)]
    (s/join "\n" (for [l lines :when (not (re-matches #"^\s*(null)?;?$" l))] l))))

(defn unquoted?
  [form]
  (and
    (isa? (class form) ::list)
    (= (first form) 'clojure.core/unquote)))

(defn eval-forms
  [form]
    (if (unquoted? form)
        (eval (second form))
        form))

(defn make-substitutions
  [locals form]
  (if 
    (and
      (unquoted? form)
      (isa? (class (second form)) clojure.lang.Symbol)) 
    (locals (second form))
    form))
 
(defn process-template-unquotes
  [locals form]
  (let [substitutions (partial make-substitutions locals)]
    (prewalk eval-forms (prewalk substitutions form))))

(defn locals-map
  [args]
  (into {} (for [a args :when (not (= '& a))] `[~(list 'quote a) ~a])))

(defn _js [forms]
  (if (> (count forms) 1)
      (apply (partial emit-local-scope ::statement) forms)
      (emit-local-scope ::statement (first forms))))

(defn unquote?
  "Tests whether the form is (unquote ...)."
  [form]
  (and (seq? form) 
       (symbol? (first form)) 
       (or
         (= (symbol (name (first form))) 'unquote)
         (= (symbol (name (first form))) 'clj))))

(defn handle-unquote [form]
  (second form))

(declare inner-walk outer-walk)

(defn inner-walk [form]
  (cond 
   (unquote? form) (handle-unquote form)
   :else (walk inner-walk outer-walk form)))

(defn outer-walk [form]
  (cond
    (symbol? form) (list 'quote form)
    (seq? form) (list* 'list form)
    :else form))

(defmacro quasiquote [form]
  (let [post-form (walk inner-walk outer-walk form)]
    post-form))

(defmacro js*
  "returns a fragment of 'uncompiled' javascript. Compile to a string using js."
  [& forms]
  (if (= (count forms) 1)
    `(quasiquote ~(first forms))
    (let [do-form `(do ~@forms)]
      `(quasiquote ~do-form))))

(defmacro cljs*
  "equivalent to (js* (clj form))"
  [form]
  `(js* (~'clj ~form)))

(defmacro cljs
  "equivalent to (js (clj form))"
  [form]
  `(js (clj ~form)))

(defmacro js 
  "takes one or more forms. Returns a string of the forms translated into javascript"
  [& forms]
  `(remove-blank-lines (_js (quasiquote ~forms))))

(declare *javascript-root*)

(defmacro js-file
  [filename & exprs]
  (let [full-path (if (bound? (var *javascript-root*))
                    (str *javascript-root* \/ filename)
                    (as-relative-path filename))]
    `(spit (as-file ~full-path) (js ~@exprs))))
