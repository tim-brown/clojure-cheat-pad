(ns cheat-pad.core
  (:require [hiccup.core :refer :all]))

;; TODO: pomogranite for live reloading

(defn blue  [x] [:cheat-quote [:span.blue x]])
(defn red   [x] [:cheat-quote [:span.red x]])
(defn green [x] [:cheat-quote [:span.green x]])

(def fancy-renderings
  (assoc {}
         `clojure.core/inc [:span.special "inc"]
         ))

         ;`blue [:span.blue [:var "%"]]
         ;`red [:span.red [:var "%"]]
         ;`green [:span.green [:var "%"]]

;; functions outside clojure.core should be considered carefully, since
;; we're just going to render this as (f "%")
(defn fancy [e]
  (cond
    (fancy-renderings e) (fancy-renderings e) 

    (fn? e) (second (e "%"))

    (and (symbol? e) (= (namespace e) "clojure.core")) [:span.command (name e)]

    (symbol? e) e))

(comment
  (cheat-sheet)
  )

(declare render-expr)

(defn render-sequence [expr cls open close]
  (-> [:span {:class cls} open]
      (into (interpose " " (map render-expr expr)))
      (conj close)))

(defn infinite-seq? [s & {:keys [infinity] :or {infinity 10}}]
  (and (seq? s) (= infinity (count (take infinity s)))))

(fancy (juxt red blue))
(fancy (comp red blue))

(defn render-expr [expr  & {:keys [infinity] :or {infinity 10}}]
  (cond
    (fancy expr) (fancy expr) 
    (nil? expr) [:tt.nil "nil"]
    (string? expr) [:tt.string "\"" expr "\""]

    (and (vector? expr) (= :cheat-quote (first expr))) (second expr)
    (vector? expr) (render-sequence expr "vector" "[" "]")

    (infinite-seq? expr :infinity infinity)
    (render-sequence (take infinity expr) "sequence" "(" "...)")

    (seq? expr)
    (render-sequence expr "sequence" "(" ")") 

    :else [:tt #_{:class (type expr)} (str expr)]))

(defn cheat [expr]
  (let [rendered-expression
        [:tr
         [:td (render-expr expr)]
         [:td " "]
         [:td (render-expr (eval expr))]]]
    (spit "rendered.html" (html rendered-expression))
    rendered-expression))

(def style-sheet
  [:style
   "body {font-size: 14pt}"
   ".blue {padding: 2px; border-radius: 0.25em; border: 3px inset blue;}"
   ".red {padding: 2px; border-radius: 0.25em; border: 3px inset red;}"
   ".green {padding: 2px; border-radius: 0.25em; border: 3px inset green;}"
   ".command {color: #000080; font-weight: bold}"
   ".special {color: blue; font-weight: bold}"
   ".nil {color: red;}"
   ])

(comment
  (cheat-sheet)
  )

(defn cheat-sheet []
  (spit "cheatsheet.html"
        (html
          [:html
           [:head
            style-sheet
            [:meta {:http-equiv "refresh" :content "2"}]
            ]
           [:body
            [:table
             (cheat `(true? 42))
             (cheat `(true? true))
             (cheat `(true? nil))
             (cheat `(true? false))
             (cheat `(juxt red blue))
             (cheat `((juxt red blue) 42))
             (cheat `((comp red blue) 42))
             (cheat `(interpose "X" [1 2 3]))
             (cheat `(mapv inc [1 2 3]))
             (cheat `(map blue [1 2 3]))
             (cheat `(repeat :hello))]]])))
