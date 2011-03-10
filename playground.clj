(ns widgets.playground
  (:use [widgets.js-gen :only (js js*)])
  (:use [gaka.core :only (css)])
  (:use [hiccup.core :only (html)])
  (:use [hiccup.page-helpers :only (javascript-tag)]))

(defn- chunk?
  [form]
  (boolean (:chunk (meta form))))

(defn widget? 
 [form]
 (boolean (:widget (meta form))))

(defn in
  [tag & more]
  (with-meta {tag more} {:chunk true}))

(defn collect-chunks
  [& exprs]
  (let [branch? (fn [b] (and (sequential? b) ((complement chunk?) b)))]
    (vec (filter chunk? (tree-seq branch? vec exprs)))))

(defn merge-chunks
  [chunks]
  (let [groups (partition-by keys (sort-by (comp name first keys) chunks))
        values (for [g groups] (apply concat (apply concat (for [i g] (vals i)))))
        tags   (for [g groups] (first (keys (first g))))
        pairs  (partition 2 (interleave tags values))]
    (for [[k v] pairs] (apply (partial in k) v))))

(defn- binding?
  ([form] (binding? form false))
  ([form inside-vec] (cond
                       (vector? form) (every? #(or (symbol? %) (binding? % true)) form)
                       (and
                         (map? form)
                         inside-vec)  (or 
                                        (every? #(or (symbol? %) (binding? % true)) (keys form))
                                        (and 
                                          (= (keys form) [:keys])
                                          (or 
                                            (and (seq? (:keys form)) (every? symbol? (:keys form)))
                                            (binding? (:keys form))))
                                        (and
                                          (= (keys form) [:keys :or])
                                          (and
                                            (or 
                                              (and (seq? (:keys form)) (every? symbol? (:keys form)))
                                              (binding? (:keys form)))
                                            (and (map? (:or form)) (every? keyword? (keys (:or form)))))))
                       :else          false)))

(defn- parse-widget-args
  [args]
  (if (not (symbol? (first args))) (throw (Exception. "The first argument to a widget should be a symbol."))
    (let [[sym & args]  args
          extract-if  (fn [pred coll] (if (pred (first coll)) coll (cons nil coll)))
          [docs & args] (extract-if string? args)
          [bvec & args] (extract-if binding? args)
          exprs         (if (= (count args) 1) (first args) args)]
      (if (empty? args) (throw (Exception. "The body of a widget can't be empty.")))
      {:name sym :docs (str docs) :bindings (or bvec []) :body args})))

(defmacro generate-widget-fn 
  [args & more] 
    `(fn ~args (merge-chunks (collect-chunks ~@more))))

(defn impure?
  [form]
  (boolean
    (cond 
      (symbol? form) (or (:impure (meta (resolve form))) (:impure (meta form)))
      (fn? form)     (:impure (meta form))
      :else          false)))

(defn- any-impure?
  [& forms]
  (some true? (map impure? (flatten forms))))

(defn memoize-with-reset 
  [f]
  (let [mem (atom {})]
    (with-meta
      (fn [& args]
        (if (= (first args) :reset)
          (reset! mem {})
          (if-let [e (find @mem args)]
            (val e)
            (let [ret (apply f args)]
              (swap! mem assoc args ret)
              ret))))
        {:memoize-atom mem})))

(defn in-css 
  [& forms]
  (let [[tag & rules] (if (keyword? (first forms)) forms (cons :css forms))]
    (in tag (apply css rules))))

(defmacro in-js 
  [& forms]
  (let [[tag & code] (if (keyword? (first forms)) forms (cons :js forms))]
    `(apply (partial in ~tag) (list (js* ~(cons 'do code))))))

(defmacro widget
  [& args]
  (let [{:keys (name docs bindings body)} (parse-widget-args args)
        impure (or (impure? name) (any-impure? body))
        func   `(generate-widget-fn ~bindings ~@body)
        memo-func `(if ~impure ~func (memoize-with-reset ~func))
        meta-data {:widget true :doc docs :impure impure}]
    `(def ~(with-meta name meta-data) (with-meta ~memo-func ~meta-data))))

(defn css-tag [s]
  [:style {:type "text/css"} (str "/*<![CDATA[*/\n" s "\n/*]]>*/")])

(defn remove-duplicates 
  [coll]
  (loop [acc [] stl coll]
    (if (empty? stl) acc
        (let [new-acc (conj acc (first stl))
              new-stl (filter (comp not (set new-acc)) stl)]
          (recur new-acc new-stl)))))

(defmacro layout
  [sym & more]
  (let [func `(fn [& views#] 
                (html
                  (let [content# (into {} (merge-chunks (collect-chunks views#)))
                        ~(symbol "put") (fn [tag#] (content# tag#))
                        ~(symbol "put-js")   (fn ([tag#] (javascript-tag (js (~(symbol "clj") (cons 'do (remove-duplicates (tag# content#)))))))
                                                 ([] (javascript-tag (js (~(symbol "clj") (cons 'do (remove-duplicates (:js content#))))))))
                        ~(symbol "put-css")  (fn ([tag#] (css-tag (clojure.string/join "\n" (remove-duplicates (tag# content#)))))
                                                 ([] (css-tag (clojure.string/join "\n" (remove-duplicates (:css content#))))))]
                    ~@more)))
        impure (impure? sym)
        memo-func `(if ~impure ~func (memoize-with-reset ~func))]
    `(def ~sym ~memo-func)))

(defmacro render
  [structure & widgets]
  `(html
    (let [content# (into {} (merge-chunks (collect-chunks ~@widgets)))
          ~(symbol "put") (fn [tag#] (content# tag#))
          ~(symbol "put-js")   (fn ([tag#] (javascript-tag (js (~(symbol "clj") (cons 'do (remove-duplicates (tag# content#)))))))
                                   ([] (javascript-tag (js (~(symbol "clj") (cons 'do (remove-duplicates (:js content#))))))))
          ~(symbol "put-css")  (fn ([tag#] (css-tag (clojure.string/join "\n" (remove-duplicates (:css content#)))))
                                   ([] (css-tag (clojure.string/join "\n" (remove-duplicates (:css content#))))))]
      ~structure)))

(widget word [text]
  (in-js :a (alert "A!"))
  (in-js :b (alert "B!")))

(layout blah
  [:head
   (put-js :a)
   (put-js :b)])
