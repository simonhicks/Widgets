(ns widgets.core
  (:use [gaka.core :only (css)])
  (:use [clojure.contrib.json :only (json-str)])
  (:use [widgets.js-gen :only (js*)])
  (:use widgets.base))

(defn in
  "Used when creating widgets to place html content at :tag"
  [tag & more]
  (with-meta {tag more} {:chunk true :widget-content :html}))

(defn in-css
  "Used when creating widgets to place css content. If the first is a keyword it 
  is treated as the tag, otherwise the content is placed at :css"
  [& forms]
  (with-meta 
    (let [[tag & rules] (if (keyword? (first forms)) 
                        forms 
                        (cons :css forms))]
      (in tag (apply css rules)))
    {:chunk true :widget-content :css}))

(defmacro in-js
  "Used when creating widgets to place js content. If the first is a keyword it 
  is treated as the tag, otherwise the content is placed at :js"
  [& forms]
  (let [[tag & code] (if (keyword? (first forms)) 
                       forms 
                       (cons :js forms))]
    `(with-meta
       (apply (partial in ~tag) 
              (list (js* ~(cons 'do code))))
       {:chunk true :widget-content :js})))

(defn merge-chunks
  "When given a flat vector of widget-chunks, merge-chunks returns a similar 
  collection of widget-chunks with content from similarly tagged widget-chunks 
  concatenated into a single widget-chunk"
  [chunks]
  (let [groups (partition-by keys (sort-by (comp name first keys) chunks))
        typs   (for [g groups] (content-type (first g)))
        tags   (for [g groups] (first (keys (first g))))
        values (for [g groups] (apply concat (apply concat (for [i g] (vals i)))))
        pairs  (partition 3 (interleave typs tags values))]
    (for [[t k v] pairs] (with-meta (apply (partial in k) v) {:chunk true :widget-content t}))))

(defmacro generate-widget-fn 
  "low-level macro for generating the function that will be assigned to a created 
  widget. Do not use this function... use (widget name ...) instead."
  [args & more] 
   `(fn ~args (merge-chunks (collect-chunks ~@more))))

(defn memoize-with-reset
  "like (memoize f) except when the returned function is evaluated with the single
  argument :reset, the cached values are all reset."
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

(defmacro widget
  "Creates a widget. Use (in :foo ...) (in-css :bar ...) and (in-js :baz ...) to add content to 
  the widget. eg.

  (widget my-widget 
    \"Really helpful and insightful doc string\"
    [some args]
    (in :title \"My Awesome Website\")
    (in :main [:div ... content])
    (in-css [:h1 :color :black])
    (in-js (alert \"Hello world!\")))

  The docstring and argument list are optional. Widgets are assumed to be pure, and are therefore
  memoized. They can be reset by passing them :reset. If a widget is impure, you can mark as such 
  using meta-data.

  (widget ^{:impure true} random-widget
    (in :main (rand)))
  
  Also see (doc layout), (doc render) and (doc render-json) for more details."
  [& args]
  (let [{:keys (name docs bindings body)} (parse-widget-args args)
        impure (or (impure? name) 
                   (any-impure? body))
        func   `(generate-widget-fn ~bindings ~@body)
        memo-func `(if ~impure 
                     ~func 
                     (memoize-with-reset ~func))
        meta-data {:widget true :doc docs :impure impure}]
    `(def ~(with-meta name meta-data) 
       (with-meta ~memo-func 
                  ~meta-data))))

(defmacro render-with-container [structure & widgets]
  "Low level macro for rendering content. Don't use this... use render or layout instead"
  `(let [content#             (into {} (merge-chunks (collect-chunks ~@widgets)))
         put-fn#              (fn [tag#] [:div {:class "tag-container" :id (str (name tag#) "-tag")} (tag# content#)])]
    (render-with put-fn# ~structure content#)))

(defn to-json-object [& widgets]
  "converts the given widgets to a hash indexed first by content type, then by tag"
  (let [render-and-merge (fn [exprs] (apply merge 
                                            (map render-chunk exprs)))
        vecs-to-map      (fn [vecs] (for [[typ exprs] vecs] 
                                         {typ (render-and-merge exprs)}))]
    (->> widgets
      (apply collect-chunks)
      (merge-chunks)
      (sort-by content-type)
      (partition-by content-type)
      (map #(vector (content-type (first %)) %))
      (vecs-to-map)
      (apply merge))))

(defmacro render
  "Allows you to place widget content within an anonymous html structure.
  eg.

  (widget word-li [text]
    (in :words [:li text]))

  (widget word-list [& words]
    (in :main 
        (render [:ul (put :words)] 
                (for [w words] (word-li w)))))

  (word-list \"hello\" \"world\")
  ({:main (\"<ul><li>hello</li><li>world</li></ul>\")})

  Any content from the widgets passed to render that is not used in a (put ...) expr 
  will not be passed on to the enclosing widget."
  [structure & widgets]
  `(let [content#             (into {} (merge-chunks (collect-chunks ~@widgets)))
         put-fn#              (fn [tag#] (tag# content#))]
    (render-with put-fn# ~structure content#)))

(defmacro layout
  "Defines a named layout. Layouts are functions that take a number of widgets 
  as arguments and return rendered html/js/css from within the widgets. This is 
  done with hiccup and the widget functions put, put-js and put-css.
  
  Within a layout, (put :tag) will expand into a seq containing the return values
  of each (in :tag ...) expr from the given widgets.
  
  (layout my-layout
    [:html
      [:head
        (put :js)]
      [:body
        (put :main)]])
  
  (widget user-widget
    [user]
    (in-js (alert \"Hi!\"))
    (in :main
      [:p (:name user)]))
  
  (my-layout (for [u users] (user-widget u)))
  ; => [:html 
  ;       [:head [:script {:type \"text/javascript\"} \"//<![CDATA[\\nalert(\\\"Hi!\\\");\\n//]]>\"]]
  ;       [:body 
  ;         [:p \"Andy\"]
  ;         [:p \"Bob\"]
  ;         [:p \"Charlie\"]]]"
  [& args]
  (let [[sym a b] args
        docs      (if (string? a) a "")
        structure (if (string? a) b a)
        func `(fn [& views#] 
                (render-with-container ~structure views#))
        impure (impure? sym)
        memo-func `(if ~impure 
                     ~func 
                     (memoize-with-reset ~func))]
    `(def ~sym 
       (with-meta ~memo-func 
                  {:impure ~impure :doc ~docs}))))

(defn render-json
  "Creates a hash containing the rendered content from the passed widgets, and returns
  this hash as a json string. The hash has up to three keys (:html, :js and :css). The 
  values stored in these keys are themselves hashes, with their contents indexed by the 
  tag originally used when the widgets were created."
  [& widgets]
  (json-str (apply to-json-object widgets)))
