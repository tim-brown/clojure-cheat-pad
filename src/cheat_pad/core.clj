(ns cheat-pad.core
  (:require
    [hiccup.core :refer :all]
    [ring.adapter.jetty :as jetty]
    ))

(declare render-expr)

(def output-file-name "output/cheatsheet.html")
(def debug-file-name "output/rendered.html")

(defn cheat-quote [x]
  (if (instance? clojure.lang.IObj x)
    (vary-meta x assoc :cheat-quoted? true)
    x))

(defn cheat-quoted? [x] (:cheat-quoted? (meta x)))

(defn blue  [x] (cheat-quote [:span.blue x]))

(defn red   [x] (cheat-quote [:span.red x]))

(defn green [x] (cheat-quote [:span.green x]))

(def illustrative-namespaces
  #{(name (ns-name *ns*))
    })

(def fancy-renderings
  {
   `clojure.core/inc [:span.special "[inc]"]
   })

;; functions outside clojure.core should be used carefully, since
;; we're just going to render this as (f "%")
(defn fancy [e]
  (cond
    (fancy-renderings e) (fancy-renderings e) 
    (fn? e) (render-expr (e "%"))
    (not (symbol? e)) nil
    (= (namespace e) "clojure.core") [:span.command (name e)]
    (illustrative-namespaces (namespace e)) (fancy @(resolve e))))

(defn render-sequence [expr cls open close]
  (-> [:span {:class cls} open]
      (into (interpose " " (map render-expr expr)))
      (conj close)))

(defn infinite-seq? [s & {:keys [infinity] :or {infinity 10}}]
  (and (sequential? s)
       (= infinity (count (take infinity s)))))

(defn render-expr [expr & {:keys [infinity] :or {infinity 10}}]
  (try
    (cond
      (fancy expr) (fancy expr)

      (nil? expr) [:tt.nil "nil"]

      (string? expr) [:tt.string "\"" expr "\""]

      (cheat-quoted? expr) expr

      (vector? expr) (render-sequence expr "vector" "[" "]")

      (infinite-seq? expr :infinity infinity) (render-sequence (take infinity expr) "sequence" "(" "...)")

      (sequential? expr) (render-sequence expr "sequence" "(" ")") 

      :else [:tt {:class (.getSimpleName (type expr))} (str expr)])

    (catch Exception e [:tt.exception (.getMessage e)])))

(def style-sheet
  [:style
   "body {font-size: 14pt}"
   ".blue {padding: 2px; border-radius: 0.25em; border: 3px inset blue;}"
   ".red {padding: 2px; border-radius: 0.25em; border: 3px inset red;}"
   ".green {padding: 2px; border-radius: 0.25em; border: 3px inset green;}"
   ".command {color: #000080; font-weight: bold}"
   ".special {color: blue; font-weight: bold}"
   ".exception {color: #440000; backgroud-color: #ffaa88}"
   ".nil {color: red;}"
   ])

(defn html-or-die [& body]
  (let [common-head [:head
        style-sheet
        [:meta {:http-equiv "refresh" :content "2"}]]]
  (try
    (html
      [:html
       common-head
       [:body body]])
    (catch Exception e
      (html
        [:html
         common-head
         [:body [:pre (.getMessage e)] [:pre (str body)]]])))))

(defn cheat [expr]
  (clojure.java.io/make-parents debug-file-name)
  (let [e (eval expr)
        rendered-expression
        [:tr
         [:td (render-expr expr)]
         [:td (render-expr e)]
         [:td.type (type e)]]]
    (spit debug-file-name (html-or-die rendered-expression))
    rendered-expression))

(defn cheat-string [string]
  (clojure.java.io/make-parents debug-file-name)
  (let [
        expr (eval (read-string string))
        rendered-expression
        [:tr
         [:td [:code string]]
         [:td (render-expr expr)]
         [:td.type (type expr)]]]
    (spit debug-file-name (html-or-die rendered-expression))
    rendered-expression))

(defn sec1 [& content] [:tr [:th {:colspan 3} content]])
(defn sec2 [& content] [:tr [:th {:colspan 3} content]])
(defn sec3 [& content] [:tr [:th {:colspan 3} content]])

(defn gen-body []
  (list
    [:table
     (sec1 "Primitives")
     (sec2 "Numbers")
     (sec3 "Literals")
     (cheat-string "7")
     (cheat-string "0xff")
     (cheat-string "017")
     (cheat-string "2r1011")
     (cheat-string "36rCRAZY")
     (cheat-string "7N")
     (cheat-string "-22/7")
     (cheat-string "2.78")
     (cheat-string "-1.2e-5")
     (cheat-string "4.2M")
     (sec1 "Random Stuff")
     (cheat `(true? 42))
     (cheat `(true? true))
     (cheat `(true? nil))
     (cheat `(true? false))
     (cheat `green)
     (cheat `(juxt red blue))
     (cheat `((juxt red blue) 42))
     (cheat `((comp red blue) 42))
     (cheat `(interpose "X" [1 2 3]))
     (cheat `(mapv inc [1 2 3]))
     (cheat `(map blue [1 2 3]))
     (cheat `(repeat :hello))]))

(defn cheat-sheet []
  (clojure.java.io/make-parents output-file-name)
  (let [content (html-or-die (gen-body))]
    (spit output-file-name content)
    content))

(comment
  (cheat-sheet)
  (render-expr (juxt red))
  (cheat `(juxt red))
  (fancy `(juxt red))
  (gen-body)
  (list
    (symbol? `green)
    (fancy `green)
    (resolve `green)
    (illustrative-namespaces (namespace `green))
    (illustrative-namespaces "cheat-pad.core")
    (green 42))
  (cheat-quote [:span.green 42])
  ((juxt red green) 42)
  (trace (render-expr ((juxt red green) 42)))
  (render-expr ((comp red green) 42)))

(defn handle-cheatsheet-request [rq]
  {
   :status 200
   :headers {"Content-type" "text/html"}
   :body (cheat-sheet)
   })

(defonce server (jetty/run-jetty #'handle-cheatsheet-request
                                 {:port 3000
                                  :join? false}))

