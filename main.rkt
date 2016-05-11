;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arul Ajmani
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz02 (read-puzzle "puzzle02.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define puzz05 (read-puzzle "puzzle05.txt"))
(define puzz06 (read-puzzle "puzzle06.txt"))
(define puzz07 (read-puzzle "puzzle07.txt"))
(define puzz08 (read-puzzle "puzzle08.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define puzz10 (read-puzzle "puzzle10.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;; (transpose g) produces the transpose of the consumed g, i.e, makes
;; rows as columns, and columns as rows.
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose empty) empty)

(define (transpose g)
  (cond
    [(empty? g) empty]
    [else 
     (local
       [;; (get-col g col) produces the consumed col of the
        ;; consumed g.
        ;; get-col: Grid Nat -> (listof Char)
        (define (get-col g col)
          (cond
            [(empty? g) empty]
            [else (cons (list-ref (first g) col)
                        (get-col (rest g) col))]))
        (define transpose-grid-rows (length (first g)))
        ;; (transpose/acc g row) produces the transpose of consumed g
        ;; with the help of accumulator row.
        ;; transpose/acc : Grid -> Grid
        (define (transpose/acc g row)
          (cond
            [(= row transpose-grid-rows) empty]
            [else (cons (get-col g row) (transpose/acc g (add1 row)))]))]
       (transpose/acc g 0))]))

;; Tests:
(check-expect (transpose '((#\A #\B #\C)))
              '((#\A) (#\B) (#\C)))
(check-expect (transpose '((#\A) (#\B) (#\C)))
              '((#\A #\B #\C)))

;; (find-wpos loc row) consumes a row from the grid and produces all the
;; wpos's present in the row, with help of consumed loc.
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))

(define (find-wpos row loc)
  (local
    [;; (first-token break? row) consumes a row and produces it's first
     ;; token as per break.
     ;; first-token: (Char -> Bool) (listof Char) -> (listof Char)
     (define (first-token break? row)
       (cond
         [(empty? row) empty]
         [(break? (first row)) empty]
         [else (cons (first row) (first-token break? (rest row)))]))
     ;; (remove-token break? row) consumes a row and removes the first token
     ;; as per the consumed break.
     ;; remove-token: (Char->Bool) (listof Char) -> (listof Char)
     (define (remove-token break? row)
       (cond
         [(empty? row) empty]
         [(break? (first row)) row]
         [else (remove-token break? (rest row))]))
     ;; (row->tokens) converts the given row to tokens, where a token is
     ;; a sub-list of #\. or #\#.
     ;; row->tokens: (listof Char) -> (listof (listof Char))
     (define (row->tokens row)
       (cond
         [(empty? row) empty]
         [(char=? (first row) #\.)
          (cons (first-token (lambda (x) (char=? x #\#)) row)
                (row->tokens (remove-token (lambda (x) (char=? x #\#)) row)))]
         [else (cons (first-token (lambda (x) (char=? x #\.)) row)
                     (row->tokens (remove-token
                                   (lambda (x) (char=? x #\.))
                                   row)))]))
     ;; (produce-wpos token-list start-pos) converts the token-list into word-pos
     ;;  with the help of start-pos to keep track of location.
     ;; produce-wpos: (listof (listof Char)) Nat -> WPos
     (define (produce-wpos token-list start-pos)
       (cond
         [(empty? token-list) empty]
         [(and (> (length (first token-list)) 1) (char=? (first (first token-list)) #\#))
          (cons (make-wpos loc
                           start-pos
                           true
                           (length (first token-list)))
                (produce-wpos (rest token-list)
                              (+ start-pos (length (first token-list)))))]
         [else (produce-wpos (rest token-list)
                             (+ start-pos (length (first token-list))))]))]
    (cond
      [(empty? row) empty]
      [else (produce-wpos (row->tokens row) 0)])))


;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; (initial-state puzzle) consumes a puzzle and produces the innitial
;; state from which we have to start searching from.
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))

(define (initial-state puzzle)
  (local [;; (cells->grid cells) consumes cells and converts them to
          ;; a grid
          ;; cells->grid: (listof string) -> Grid
          (define (cells->grid cells)
            (cond
              [(empty? cells) empty]
              [else (cons (string->list (first cells))
                          (cells->grid (rest cells)))]))
          (define grid (cells->grid (first puzzle)))
          ;; (grid-wpos g row) consumes g to produce
          ;; all the horizontal wpos's in the grid g row by row.
          ;; grid-wpos: Grid (listof Char) -> (listof WPos)
          (define (grid-wpos g row)
            (cond
              [(empty? g) empty]
              [else (append (find-wpos (first g) row)
                            (grid-wpos (rest g) (add1 row)))]))
          (define transpose-grid (transpose grid))
          ;; (convert-horiz-to-vert wpos-list) consumes a wpos-list having
          ;; having horizontal wpos, and makes them vertical.
          ;; convert-horiz-to-vert: (listof WPos) -> (listof WPos)
          (define (convert-horiz-to-vert wpos-list)
            (cond
              [(empty? wpos-list) empty]
              [else (cons (flip (first wpos-list))
                          (convert-horiz-to-vert (rest wpos-list)))]))]
    (make-state grid
                (append (grid-wpos grid 0)
                        (convert-horiz-to-vert (grid-wpos transpose-grid 0)))
                (second puzzle))))
;; Tests:


;; (extract-wpos g wp)
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local
    [;; (get-col g col) produces the consumed col of the
     ;; consumed g.
     ;; get-col: Grid Nat -> (listof Char)
     (define (get-col g col)
       (cond
         [(empty? g) empty]
         [else (cons (list-ref (first g) col)
                     (get-col (rest g) col))]))
     ;; (get-row g row) produces the consumed row of the g.
     ;; get-row: Grid Nat -> (listof Char)
     (define (get-row g row)
       (cond
         [(= 0 row) (first g)]
         [else (get-row (rest g) (sub1 row))]))
     ;; (get-wpos start end row/col) consumes a start and end to produce
     ;; a sub-list of the row/col.
     ;; get-wpos: Nat Nat (listof Char) -> (listof Char)
     ;; Requires: end >= start
     (define (get-wpos start end row/col)
       (cond
         [(zero? end) (cons (first row/col) empty)]
         [(zero? start) (cons (first row/col)
                              (get-wpos 0 (sub1 end) (rest row/col)))]
         [else (get-wpos (sub1 start) (sub1 end) (rest row/col))]))]
    (cond
      [(wpos-horiz? wp)
       (get-wpos (wpos-col wp)
                 (sub1 (+ (wpos-len wp) (wpos-col wp)))
                 (get-row g (wpos-row wp)))]
      [else (get-wpos (wpos-row wp)
                      (sub1 (+ (wpos-len wp) (wpos-row wp)))
                      (get-col g (wpos-col wp)))])))

;; Tests:

;; (replace-wpos g wp loc) replaces the consumed loc in the wp of the g,
;; producing a new version of g, thus formed.
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local
    [;; (horiz-replace g-row start end pos loc) replaces the g-row
     ;; with the consumed loc from start till end, with help of pos to
     ;; keep track of position.
     ;; horiz-replace: (listof Char) Nat Nat Nat (listof Char) -> (listof Char)
     (define (horiz-replace g-row start end pos loc)
       (cond
         [(empty? g-row) empty]
         [(and (>= pos start) (<= pos end))
          (cons (first loc)
                (horiz-replace (rest g-row) start end (add1 pos) (rest loc)))]
         [else (cons (first g-row)
                     (horiz-replace (rest g-row) start end (add1 pos) loc))]))
     ;; (vert-replace g start end pos loc) vertically replaces loc from start to
     ;; end in the consumed g using pos for position tracking.
     ;; vert-replace: Grid Nat Nat Nat (listof Char) -> Grid
     (define (vert-replace g start end pos loc)
       (cond
         [(empty? g) empty]
         [(and (>= pos start) (<= pos end))
          (cons (horiz-replace (first g)  (wpos-col wp) (wpos-col wp) 0 loc)
                (vert-replace (rest g) start end (add1 pos) (rest loc)))]
         [else (cons (first g)
                     (vert-replace (rest g) start end (add1 pos) loc))]))
     ;; (produce-horiz-grid g replace-row pos start end) consumes a g and
     ;; replaces the replace row from start to end keeping track with pos.
     ;; produce-horiz-grid: Grid (listof Char) Nat Nat Nat -> Grid
     (define (produce-horiz-grid g replace-row pos start end)
       (cond
         [(empty? g) empty]
         [(= pos replace-row)
          (cons (horiz-replace (first g) start end 0 loc)
                (produce-horiz-grid (rest g) replace-row (add1 pos) start end))]
         [else (cons (first g)
                     (produce-horiz-grid
                      (rest g) replace-row (add1 pos) start end))]))]
    (cond
      [(wpos-horiz? wp)
       (produce-horiz-grid g (wpos-row wp) 0 (wpos-col wp)
                           (sub1 (+ (wpos-len wp) (wpos-col wp))))]
      [else (vert-replace g (wpos-row wp)
                          (sub1 (+ (wpos-len wp) (wpos-row wp))) 0 loc)])))


;; Tests:


;; (fit? word cells) produces whether the consumed word can be fit
;; in the consumed cells.
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond
    [(not (= (length word) (length cells))) false]
    [(and (empty? word) (empty? cells)) true]
    [(or (char=? (first word) (first cells)) (char=? (first cells) #\#))
     (fit? (rest word) (rest cells))]
    [else false]))

;; Tests:
(check-expect (fit? (string->list "LOL") (string->list "L#L#")) false)
(check-expect (fit? (string->list "CAT") (string->list "###")) true)


;; (neighbours s)
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local
    [;; (letters loc) counts the number of letters in loc
     ;; letters: (listof Char) -> Nat
     (define (letters loc)
       (foldl (lambda (x y)
                (cond
                  [(not (char=? x #\#)) (add1 y)]
                  [else y]))
              0
              loc))
     (define grid (state-grid s))
     ;; (choose-wpos lo-wpos) consumes lo-wpos and chooses one of it
     ;; as per number of letters.
     ;; choose-wpos: (listof WPos) -> WPos
     (define (choose-wpos lo-wpos)
       (foldl (lambda (x y)
                (cond
                  [(> (letters (extract-wpos grid x)) (letters (extract-wpos grid y))) x]
                  [else y]))
              (first lo-wpos)
              lo-wpos))
     (define chosen-wpos (choose-wpos (state-positions s)))
     ;; (remove-chosen-wpos lo-wpos) removes the chosen wpos from lo-wpos.
     ;; remove-chosen-wpos: (listof WPos) -> (listof WPos)
     (define (remove-chosen-wpos lo-wpos)
       (cond
         [(equal? chosen-wpos (first lo-wpos)) (rest lo-wpos)]
         [else (cons (first lo-wpos) (remove-chosen-wpos (rest lo-wpos)))]))
     ;; (remove-word to-remove lo-words) removes the to-remove from lo-words.
     ;; remove-words: Str (listof Str) -> (listof Str)
     (define (remove-word to-remove lo-words)
       (cond
         [(string=? (first lo-words) to-remove) (rest lo-words)]
         [else (cons (first lo-words) (remove-word to-remove (rest lo-words)))]))
     ; WPOS has been chosen, now test each word of the state.
     ;; (possible-state words) produces all the possible next states from
     ;; the consumed words.
     ;; possible-state: (listof Str) -> (listof State)
     (define (possible-state words)
       (cond
         [(empty? words) empty]
         [(fit? (string->list (first words)) (extract-wpos grid chosen-wpos)) (cons (make-state (replace-wpos grid
                                                                                                              chosen-wpos
                                                                                                              (string->list (first words)))
                                                                                                (remove-chosen-wpos (state-positions s))
                                                                                                (remove-word (first words) (state-words s)))
                                                                                    (possible-state (rest words)))]
         [else (possible-state (rest words))]))]
    (possible-state (state-words s))))


;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("RRR" "DOG" "LOL")))
              empty)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

 (check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

(check-expect (criss-cross puzz10)
              (list
               "ALLEGIANCES............"
               "R.A.A...H.TOWNCRIER.J.V"
               "M.D.L..MACE.A.A...E.O.I"
               "O.I.L.C.I.E.T.S.KNIGHTS"
               "U.E.A.O.N.D.TITHE.N.N.O"
               "R.S.N.I.M...L.L.E.S...R"
               "Y...T.FLAGS.E.E.P..DOGS"
               ".SWORDS.I.E....D...R..."
               "A.I.Y...LORDS.WENCHES.."
               "R.L..C.H..F.A..E...S..."
               "R.D.SHIELDS.X..D.G.S..."
               "O.B..A.R.R.MOATS.U..C.."
               "WOOL.R.A.A..N.E..I..O.."
               "..A..G.L.W....NOBLEMAN."
               "..R..E.D.B....T..D..T.."
               ".....R..CRESTS.F..CLOAK"
               "J..P...B.I....MANOR.F.I"
               "OLDENGLAND.....I..O.A.N"
               "U..A...T.G.SQUIRE.S.R.G"
               "S..S...T.E.P...S..S.M.."
               "T..A...L...U......B.S.."
               ".LANCE.E...R..FLAGON..."
               "...T.......S......W...."))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window



