(ns widgets.test.all
  (:use clojure.test)
  (:use [clojure.contrib.json :only (read-json)])
  (:use [clojure.contrib.str-utils :only (re-gsub)])
  (:use [hiccup.page-helpers :only (javascript-tag)])
  (:use [hiccup.core :only (html)])
  (:use [gaka.core :only (css)])
  (:use [widgets.js-gen :only (js js*)])
  (:use widgets.base :reload)
  (:use widgets.core :reload))

(def test-function 
  (fn [] "a"))

(def test-widget 
  (with-meta 
    (fn [] "a") 
    {:widget true}))

(deftest widget?-test
 (testing "widget?"
   (testing "should correctly identify something marked as a widget"
     (is (widget? test-widget))
     (is (widget? (with-meta {:a \a} {:widget true}))))

   (testing "shouldn't incorrectly identify a widget"
     (is (not (widget? test-function)))
     (is (not (widget? {:a \a}))))))

(deftest in-test
  (testing "in"
    (testing "should return a hash-map"
      (is (= (class (in :test \a)) 
             (class {})))

      (testing "with one key"
        (is (= :the-key 
               (first (keys (in :the-key "doesn't matter")))))
               
        (is (= 1 (count (keys (in :only-one "blah blah blah"))))))

      (testing "with one list containing all of the values"
        (is (= 1 (count (vals (in :only-one \a \s \d \f)))))

        (is (= 4 (count (:four (in :four \a \s \d \f)))))

        (is (= '(\a \s \d \f) 
               (:list (in :list \a \s \d \f))))))))

(deftest in-js-test
  (testing "in-js"
    (testing "should take a javascript s-expr without complaining"
      (is (in-js :the-key (var x 1))))

    (testing "should return a hash-map"
      (is (= (class (in-js :the-key (var x 1)))
             (class {})))

      (testing "with the given tag as the only key"
        (is (= :the-key
               (first (keys (in-js :the-key (var x 1))))))

        (is (= 1 (count (keys (in-js :the-key (var x 1))))))

        (testing "which should default to :js"
          (is (= :js
                 (first (keys (in-js (var x 1))))))))

      (testing "where the value is a list containing the un-compiled js exprs"
          (is (= (js* (do (var x 1)))
                 (first (:js (in-js (var x 1))))))))))
      

(deftest in-css-test
  (testing "in-css"
    (testing "should return a hash-map"
      (is (= (class (in-css :the-key ["h1" :color "#000"]))
             (class {})))

      (testing "with the given tag as the only key"
        (is (= :the-key
               (first (keys (in-css :the-key ["h1" :color :purple])))))

        (is (= 1 (count (keys (in-css :the-key ["h1" :color :maroon]))))))

        (testing "which should default to :css"
          (is (= :css
                 (first (keys (in-css ["p" :color :fluorescent-pink]))))))

      (testing "where the value is a list containing the rendered css"
        (is (= (css ["h1" :color "#000"] ["h2" :color "#fff"])
               (first (:the-key (in-css :the-key ["h1" :color "#000"] ["h2" :color "#fff"])))))))))

(deftest binding?-test
  (testing "binding?"
    (testing "should recognise bindings"
      (testing "with only symbols."
        (is (binding? '[a s d & f]))
        (is (binding? '[[a s] d])))

      (testing "with literals"
        (is (not (binding? 1)))
        (is (not (binding? :a)))
        (is (not (binding? "a")))
        (is (not (binding? 'a)))
        (is (not (binding? '[1 2 3 4])))
        (is (not (binding? '["A" "S" "D"])))
        (is (not (binding? '[:a :s :d :f]))))

      (testing "with map destructuring."
        (is (binding? '[{a :a b :b}]))
        (is (binding? '[{:keys [a b c]}]))
        (is (binding? '[{:keys (a b c)}])))

      (testing "with irritating stuff that's designed to f*** up my implementation."
        (is (not (binding? '[(a b c)])))
        (is (not (binding? '[{:keys [1 2 3]}])))
        (is (not (binding? '{:keys [a b c]}))))

      (testing "with deeply nested, complex destructuring"
        (is (binding? '[{{:keys [[a s] d {:keys [f]}]} :map}]))
        (is (binding? '[{name :name [hole1 hole2] :scores {:keys [[a s] d {:keys [f]}]} :map}]))
        (is (binding? '[{:keys [a s d] :or {:a 1 :b 2 :c 3}}]))
        (is (binding? '[{name :name [hole1 hole2] :scores}]))))))

(deftest parse-widget-args-test
  (testing "parse-widget-args"
    (testing "should correctly extract all parts"
      (testing "when all arguments are included"
        (let [parsed-args (parse-widget-args ['bob '"docs" '[a b c] (list test-function) (list test-widget)])]
          (is (= 'bob 
                 (:name parsed-args)))

          (is (= "docs" 
                 (:docs parsed-args)))

          (is (= '[a b c] 
                 (:bindings parsed-args)))

          (is (= (list (list test-function) (list test-widget)) 
                 (:body parsed-args)))))

      (testing "when the docs are left out"
        (let [parsed-args (parse-widget-args ['bob '[a b c] (list test-function) (list test-widget)])]
          (is (= 'bob 
                 (:name parsed-args)))

          (is (= "" 
                 (:docs parsed-args)))

          (is (= '[a b c] 
                 (:bindings parsed-args)))

          (is (= (list (list test-function) (list test-widget)) 
                 (:body parsed-args)))))

      (testing "when the bindings are left out"
        (let [parsed-args (parse-widget-args ['bob "docs" (list test-function) (list test-widget)])]
          (is (= 'bob 
                 (:name parsed-args)))

          (is (= "docs" 
                 (:docs parsed-args)))

          (is (= [] 
                 (:bindings parsed-args)))

          (is (= (list (list test-function) (list test-widget)) 
                 (:body parsed-args)))))

      (testing "when the docs and the bindings are left out"
        (let [parsed-args (parse-widget-args ['bob (list test-function) (list test-widget)])]
          (is (= 'bob 
                 (:name parsed-args)))

          (is (= "" 
                 (:docs parsed-args)))

          (is (= [] 
                 (:bindings parsed-args)))

          (is (= (list (list test-function) (list test-widget)) 
                 (:body parsed-args))))))

    (testing "should complain when the first arg isn't a symbol"
      (is (thrown-with-msg? Exception #"should be a symbol" (parse-widget-args ['"docs" '[a b c] (list test-function) (list test-widget)])))
      (is (thrown-with-msg? Exception #"should be a symbol" (parse-widget-args ['[a b c] (list test-function) (list test-widget)])))
      (is (thrown-with-msg? Exception #"should be a symbol" (parse-widget-args [(list test-function) (list test-widget)]))))

    (testing "should complain when the body is empty"
      (is (thrown-with-msg? Exception #"can't be empty" (parse-widget-args ['bob '"docs" '[a b c]])))
      (is (thrown-with-msg? Exception #"can't be empty" (parse-widget-args ['bob '[a b c]])))
      (is (thrown-with-msg? Exception #"can't be empty" (parse-widget-args ['bob '"docs"])))
      (is (thrown-with-msg? Exception #"can't be empty" (parse-widget-args ['bob]))))))

(widget test1 
  [] 
  nil)

(widget widget-with-binding 
  [a] 
  (in :a a))

(widget widget-without-binding 
  (in :b \b))

(widget widget-with-doc "Testing docs" 
  [] 
  (in :a 1))

(widget widget-without-doc 
  [] 
  (in :a 1))

(widget test2 
  (in :a 1) 
  (in :b \b))

(widget test2b 
  (widget-with-binding 1) 
  (widget-without-binding))

(widget test-widget-merge 
  (widget-with-binding 1) 
  (widget-with-binding 2) 
  (widget-with-binding 3))

(widget nesting1 
  (test2b))

(widget nesting2 
  (nesting1))

(widget nesting3 
  (nesting2))

(widget nesting4 
  (nesting3))

(widget nesting5 
  (nesting4))

(widget nesting6 
  (nesting5))

(widget with-lazy-seq 
  (for [i (range 1 6)] 
    (widget-with-binding i)))

(widget without-lazy-seq 
  (widget-with-binding 1) 
  (widget-with-binding 2) 
  (widget-with-binding 3) 
  (widget-with-binding 4) 
  (widget-with-binding 5))

(deftest widget-test
  (testing "widget"
    (testing "should produce widgets"
      (is (widget? test1)))

    (testing "should work with or without bindings"
      (is (widget? widget-with-binding))

      (is (widget? widget-without-binding)))

    (testing "should work with or without a docstring"
      (is (= (widget-with-doc) 
             (widget-without-doc)))

      (is (= (:doc (meta widget-with-doc)) "Testing docs"))))

  (testing "widgets"
    (testing "should collect (in ...) forms together"
      (is (= (list {:a (list 1)} {:b (list \b)}) 
             (test2))))

    (testing "should treat nested widgets the same as their component parts"
      (is (= (test2) 
             (test2b))))

    (testing "should merge the content from similar tags into lists, preserving their original order"
      (is (= (list {:a (list 1 2 3)}) 
             (test-widget-merge))))

    (testing "should work with arbitrary levels of nesting"
      (is (= (nesting1) 
             (nesting6))))

    (testing "should treat lazy-sequences sensibly"
      (is (= (with-lazy-seq) 
             (without-lazy-seq))))))

(defn remove-unecessary-ws [s]
  (->> s 
    (re-gsub #"\n+" "\n") 
    (re-gsub #" +" " ")))

(deftest css-tag-test
  (testing "css-tag"
    (testing "should produce a valid css-tag"
      (is (= (remove-unecessary-ws (html (css-tag (css [:p {:color :red}])))) 
             "<style type=\"text/css\">/*<![CDATA[*/\np {\n color: red;}\n/*]]>*/</style>")))))

(widget render-test-widget
  (in :hello "hello")
  (in :world "world")
  (in-css 
    ["div" {:width "100px"}])
  (in-css :more-css 
          ["div" {:height "100px"}])
  (in-js (set! x 1))
  (in-js :more-js 
         (set! y 2))
  (in-js :onload
         (. ($ "hide-me") hide)))

(widget word-li 
  [text] 
  (in :words 
      [:li text]))

(widget word-list 
  [& words]
  (in :main 
    (render [:ul (put :words)] 
            (for [w words] 
              (word-li w)))))

(deftest render-test
  (testing "render"
    (testing "should place widgets within an html structure"
      (is (= "<h1>hello</h1>" 
             (render [:h1 (put :hello)] 
                     (render-test-widget))))

      (is (= "<div><h1>hello</h1><h2>world</h2></div>" 
             (render [:div 
                      [:h1 (put :hello)] 
                      [:h2 (put :world)]] 
                     (render-test-widget)))))

    (testing "should render css using put-css"
      (is (= (html (css-tag (css [:div {:width "100px"}]))) 
             (render (put-css) (render-test-widget))))

      (is (= (html (css-tag (css [:div {:height "100px"}]))) 
             (render (put-css :more-css) (render-test-widget)))))

    (testing "should render js using put-js"
      (is (= (html (javascript-tag (js (set! x 1)))) 
             (render (put-js) (render-test-widget))))

      (is (= (html (javascript-tag (js (set! y 2)))) 
             (render (put-js :more-js) (render-test-widget))))

      (is (= (html (javascript-tag (js (set! window.onload (fn [] (. ($ "hide-me") hide))))))
             (render (onload) (render-test-widget)))))

    (testing "should work when nested in a widget"
      (is (= "<ul><li>foo</li><li>bar</li><li>baz</li></ul>" 
             (render (put :main) (word-list "foo" "bar" "baz")))))))

(layout test-layout1 
  [:h1 (put :hello)])

(layout test-layout-with-docs
  "Testing layout docs"
  [:h1 (put :hello)])

(layout css-only 
  (put-css))

(layout js-only 
  (put-js))

(deftest layout-test
  (testing "layout"
    (testing "should place html content from widgets inside an html structure, with a div for each (put :tag ...) expr"
      (is (= (render [:h1 [:div#hello-tag.tag-container (put :hello)]] (render-test-widget))
             (test-layout1 (render-test-widget)))))

    (testing "should treat js/css content the same way that render does"
      (is (= (render (put-js) (render-test-widget))
             (js-only (render-test-widget))))
      
      (is (= (render (put-css) (render-test-widget))
             (css-only (render-test-widget)))))

    (testing "should work with or without a doc-string"
      (is (= (test-layout1 (render-test-widget)) 
             (test-layout-with-docs (render-test-widget))))

      (is (= "Testing layout docs" 
             (:doc (meta test-layout-with-docs)))))))

(def i-am-impure :blah)

(def i-am-impure-too
  (with-meta
    [\b \l \a\h]
    {:impure true}))

(deftest impure?-test
  (testing "impure?"
    (testing "should correctly identify a symbol marked as impure"
      (is (impure? (with-meta 'i-am-impure {:impure true}))))

    (testing "should correctly identify a symbol bound to something that is marked as impure"
      (is (impure? i-am-impure-too)))

    (testing "should correctly identify a function that is marked as impure"
      (is (impure? (with-meta (fn [] nil) {:impure true}))))

    (testing "shouldn't say anything that isn't impure is impure"
      (is (not (impure? (fn [] nil))))

      (is (not (impure? [\b \l \a \h])))

      (is (not (impure? 'i-am-not-impure))))))

(widget ^{:impure true} explicitly-impure
  (in :main 
    [:p (rand 10)]))

(widget implicitly-impure
  (explicitly-impure))

(layout ^{:impure true} impure-layout
  [:div
   [:h1 (str "Random number of the day: " (rand))]
   (put :main)])

(deftest impurity-marking-test
  (testing "widget"
    (testing "should mark a widget as impure when instructed to"
      (is (impure? explicitly-impure)))

    (testing "should mark a widget as impure when it contains another impure widget"
      (is (impure? implicitly-impure))))

  (testing "layout"
    (testing "should mark a layout as impure when instructed to"
      (is (impure? impure-layout)))))

(widget memoized-rand [n]
  (in :rand 
      (rand n)))

(widget ^{:impure true} impure-rand [n]
  (in :rand 
      (rand n)))

(widget widget-with-a [a] (in :a a))

(layout memoized-layout
  [:div
   [:p (rand)]
   (put :a)])

(layout ^{:impure true} impure-layout
  [:div
   [:p (rand)]
   (put :a)])

(deftest memoization-test
  (testing "widget"
    (testing "should automatically memoize widgets"
      (is (= (memoized-rand 1) 
             (memoized-rand 1) 
             (memoized-rand 1) 
             (memoized-rand 1)))

      (is (not (= (memoized-rand 2) 
                  (memoized-rand 3) 
                  (memoized-rand 4))))

      (testing "unless they're marked as impure"
        (is (not (= (impure-rand 1) 
                    (impure-rand 1) 
                    (impure-rand 1) 
                    (impure-rand 1))))))

    (testing "memoized widgets should reset, when called with :reset"
      (is (not (= (memoized-rand 1) 
                  (do 
                    (memoized-rand :reset) 
                    (memoized-rand 1)))))))

  (testing "layout"
    (testing "should automatically memoize layouts"
      (is (= (memoized-layout (widget-with-a 1)) 
             (memoized-layout (widget-with-a 1)) 
             (memoized-layout (widget-with-a 1)) 
             (memoized-layout (widget-with-a 1))))

      (is (not (= (memoized-layout (widget-with-a 2)) 
                  (memoized-layout (widget-with-a 3)) 
                  (memoized-layout (widget-with-a 4)) 
                  (memoized-layout (widget-with-a 5)))))
             
      (testing "unless they're marked as impure"
        (is (not (= (impure-layout (widget-with-a 1)) 
                    (impure-layout (widget-with-a 1)) 
                    (impure-layout (widget-with-a 1)) 
                    (impure-layout (widget-with-a 1)))))))

    (testing "memoized layouts should reset, when called with :reset"
      (is (not (= (memoized-layout (widget-with-a 1)) 
                  (do 
                    (memoized-layout :reset) 
                    (memoized-layout (widget-with-a 1)))))))))

(widget h1-red 
  (in-css 
    ["h1" 
     {:color "#f00"}]))

(widget h2-green 
  (in-css 
    ["h2" 
     {:color "#0f0"}]))

(widget h1-h2 
  (in-css 
    ["h1" 
     {:color "#f00"}] 
    ["h2" 
     {:color "#0f0"}]))

(widget x-1 
  (in-js 
    (var x 1)))

(widget y-2 
  (in-js 
    (var y 2)))

(widget x1y2 
  (in-js 
    (var x 1) 
    (var y 2)))

(deftest duplicate-removal-test
  (testing "render and layout"
    (testing "should include multiple css rules from multiple widgets"
      (is (= (html (css-tag (css ["h1" {:color "#f00"}] ["h2" {:color "#0f0"}]))) 
             (css-only (h1-red) (h2-green)) 
             (render (put-css) (h1-red) (h2-green))))

      (testing "even if they both include an identical rule"
        (is (= (html (css-tag (css ["h1" {:color "#f00"}] ["h1" {:color "#f00"}] ["h2" {:color "#0f0"}]))) 
               (css-only (h1-red) (h1-h2)) 
               (render (put-css) (h1-red) (h1-h2)))))

      (testing ", unless the widgets are the same."
        (is (= (html (css-tag (css ["h1" {:color "#f00"}]))) 
               (css-only (h1-red) (h1-red)) 
               (render (put-css) (h1-red) (h1-red))))))

    (testing "should include multiple js rules from multiple widgets"
      (is (= (html (javascript-tag (js (var x 1) (var y 2)))) 
             (js-only (x-1) (y-2)) 
             (render (put-js) (x-1) (y-2))))

      (testing "even if they both include an identical expression"
        (is (= (html (javascript-tag (js (var x 1) (var x 1) (var y 2)))) 
               (js-only (x-1) (x1y2)) 
               (render (put-js) (x-1) (x1y2)))))

      (testing ", unless the widgets are the same."
        (is (= (html (javascript-tag (js (var x 1)))) 
               (js-only (x-1) (x-1)) 
               (render (put-js) (x-1) (x-1))))))))

(deftest content-type-test
  (testing "in, in-js and in-css"
    (testing "should add :widget-content to the returned object's meta-data"
      (is (= (:widget-content (meta (in :test \a))) :html))

      (is (= (:widget-content (meta (in-css :some-styles ["h1" :color :orange]))) :css))

      (is (= (:widget-content (meta (in-js :whatever (var x 1)))) :js))))

  (testing "content-type"
    (testing "should extract the content-type from a chunk"
      (is (= (content-type (in :test \a)) :html))

      (is (= (content-type (in-css :some-styles ["h1" :color :orange])) :css))

      (is (= (content-type (in-js :whatever (var x 1))) :js)))

    (testing "should return nil for other objects"
      (is (= (content-type {:a \a}) nil)))

    (testing "should be maintained after passing through merge-chunks"
      (is (= (content-type (in-css :some-styles ["h1" :color :orange]))
             (content-type (in-css :some-styles ["h2" :color "#000"]))
             (content-type (first 
                             (merge-chunks (collect-chunks 
                               (in-css :some-styles ["h1" :color :orange])
                               (in-css :some-styles ["h2" :color "#000"]))))))))))

(deftest to-json-object-test
  (testing "to-json-object"
    (testing "should return a hash with one key for each content type present"
      (is (= (sort (keys (to-json-object (render-test-widget))))
             (sort (list :html :css :js))))

      (is (= (keys (to-json-object (word-list "foo" "bar" "baz")))
             (list :html)))

      (testing "which should each contain a hash mapping tags to rendered content"
        (is (= (render (put :hello) (render-test-widget))
               (:hello (:html (to-json-object (render-test-widget))))))
        
        (is (= (render (put-js :more-js) (render-test-widget))
               (html (javascript-tag (:more-js (:js (to-json-object (render-test-widget))))))))

        (is (= (render (put-css :more-css) (render-test-widget))
               (html (css-tag (:more-css (:css (to-json-object (render-test-widget))))))))))))

(deftest render-json-test
  (testing "render-json"
    (testing "should return a valid json string"
      (is (string? (render-json (render-test-widget))))
  
      (testing "that converts back to the appropriate json object"
        (is (= (read-json (render-json (render-test-widget)))
               (to-json-object (render-test-widget))))))))


          

(run-tests)
