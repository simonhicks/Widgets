(ns widgets.base
  (:use [widgets.js-gen :only (js cljs)])
  (:use [clojure.string :only (join)])
  (:use [hiccup.core :only (html)])
  (:use [hiccup.page-helpers :only (javascript-tag)]))

(defn chunk?
  [form]
  (boolean (:chunk (meta form))))

(defn widget? 
  [form]
  (boolean (:widget (meta form))))

(defn content-type
  [form]
  (:widget-content (meta form)))

(defn collect-chunks
  "When given a nested collection of widget-chunks (as produced by (in tag & more)) 
  collect-chunks flattens the entire tree to a simple vector of widget-chunks"
  [& exprs]
  (let [branch? (fn [b] 
                  (and (sequential? b) 
                       ((complement chunk?) b)))]
    (vec (filter chunk? (tree-seq branch? vec exprs)))))

(defn impure?
  "Returns true if the given form is tagged as impure."
  [form]
  (boolean 
    (cond 
      (symbol? form) (or (:impure (meta (resolve form))) 
                         (:impure (meta form)))
      :else          (:impure (meta form)))))

(defn any-impure?
  [& forms]
  (some true? (map impure? (flatten forms))))

(defn binding?
  "returns true if form is a valid binding for use in a defn or fn"
  ([form] (binding? form false))
  ([form inside-vec] (cond
                       (vector? form) (every? #(or (symbol? %) 
                                                   (binding? % true)) 
                                              form)
                       (and
                         (map? form)
                         inside-vec)  (or 
                                        (every? #(or (symbol? %) 
                                                     (binding? % true)) 
                                                (keys form))
                                        (and 
                                          (= (keys form) [:keys])
                                          (or 
                                            (and (seq? (:keys form)) 
                                                 (every? symbol? (:keys form)))
                                            (binding? (:keys form))))
                                        (and
                                          (= (keys form) [:keys :or])
                                          (and
                                            (or 
                                              (and (seq? (:keys form)) 
                                                   (every? symbol? (:keys form)))
                                              (binding? (:keys form)))
                                            (and (map? (:or form)) 
                                                 (every? keyword? (keys (:or form)))))))
                       :else          false)))

(defn parse-widget-args
  "extracts the name, docstring, binding form and body expressions from a set of 
  args passed to the widget macro"
  [args]
  (if (not (symbol? (first args))) (throw (Exception. "The first argument to a widget should be a symbol."))
    (let [[sym & args]  args
          extract-if  (fn [pred coll] (if (pred (first coll)) 
                                        coll 
                                        (cons nil coll)))
          [docs & args] (extract-if string? args)
          [bvec & args] (extract-if binding? args)
          exprs         (if (= (count args) 1) 
                          (first args) 
                          args)]
      (if (empty? args) 
        (throw (Exception. "The body of a widget can't be empty.")))
      {:name sym :docs (str docs) :bindings (or bvec []) :body args})))

(defn remove-duplicates
  "returns a vector containing all the elements from coll with duplicates removed, 
  maintaining the order in which they first appeared."
  [coll]
  (loop [acc [] stl coll]
    (if (empty? stl) acc
        (let [new-acc (conj acc (first stl))
              new-stl (filter (comp not (set new-acc)) stl)]
          (recur new-acc new-stl)))))

(defn expand-js
  "removes duplicates and renders javascript s-exprs. Returns a js string"
  [exprs]
  (cljs (cons 'do (remove-duplicates exprs))))

(defn expand-css
  "removes duplicates and joins css strings. Returns a single css string"
  [exprs]
  (join (remove-duplicates exprs)))

(defn css-tag [s] 
  [:style {:type "text/css"} (str "/*<![CDATA[*/\n" s "\n/*]]>*/")])

(defmacro render-with
  [put-fn structure content]
  `(html
     (let [~'put      ~put-fn
           ~'put-js   (fn ([tag#] (javascript-tag (expand-js (tag# ~content))))
                                    ([]     (javascript-tag (expand-js (:js  ~content)))))
           ~'onload   (fn [] 
                                  (javascript-tag (js 
                                      (set! window.onload 
                                            (~'fn [] (~'clj (cons 'do (:onload ~content))))))))
           ~'put-css  (fn 
                                  ([tag#] (css-tag (expand-css (tag# ~content))))
                                  ([]     (css-tag (expand-css (:css ~content)))))]
       ~structure)))

(defn render-chunk
  [form]
  (let [typ             (content-type form)
        [[tag content]] (vec form)
        rendered        (case typ
                          :html (html content)
                          :js   (expand-js content)
                          :css  (expand-css content)
                          content)]
    {tag rendered}))
