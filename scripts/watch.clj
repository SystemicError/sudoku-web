(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'sudoku-web.core
   :output-to "out/sudoku_web.js"
   :output-dir "out"})
