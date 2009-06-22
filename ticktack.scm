(use srfi-1)
(use srfi-27)
(use util.list)

;;乱数の初期化
(random-source-randomize! default-random-source)

;;二つのリストの積集合を返す
(define (intersection lst1 lst2)
  (define (iter lst1 lst2 result)
    (if (null? lst1) result
        (if (memq (car lst1) lst2)
            (iter (cdr lst1) lst2 (cons (car lst1) result))
            (iter (cdr lst1) lst2 result))))
  (iter lst1 lst2 '()))

;;ボードの生成
(define (make-board n)
  (iota (* n n) 1))

;;勝利パターンの生成
(define (make-win-patterns n)
  (define (verticalize m)
    (if (null? (car m)) '()
        (cons (map car m) (verticalize (map cdr m)))))
  (let ((horizontal-patterns (slices (make-board n) n)))
    (cons (iota n 1 (+ n 1))
          (cons (iota n n (- n 1))
                (append horizontal-patterns
                        (verticalize horizontal-patterns))))))

;;プレーヤのコンストラクタ
(define (make-player name)
  (cons name '()))

;;プレーヤのセレクタ
(define (name player)
  (car player))

(define (pieces player)
  (cdr player))

;;勝利判定
(define (win? pieces n win-patterns)
  (if (null? win-patterns) #f
      (if (eq? n
               (length (intersection pieces (car win-patterns))))
          #t
          (win? pieces n (cdr win-patterns)))))

;;盤から一つ番号を選ぶ
(define (choice player board)
  (let ((rnd (random-integer (length board))))
    (values
     (cons (name player) (cons (list-ref board rnd) (pieces player)))
     (append (take board rnd) (cdr (drop board rnd))))))
 
;;決着するまで回数を重ねる
(define (fight name1 name2 n)
  (define (loop player-a player-b board win-patterns)
    (if (null? board)
        (cond [(win? (pieces player-a) n win-patterns) (name player-a)]
              [(win? (pieces player-b) n win-patterns) (name player-b)]
              [else 'draw])
        (receive (pl bo) (choice player-a board)
          (if (win? (pieces pl) n win-patterns)
              (name player-a)
              (loop player-b pl bo win-patterns)))))
  (loop (make-player name1)
        (make-player name2)
        (make-board n)
        (make-win-patterns n)))

;;指定回数ゲーム実行
(define (run name1 name2 n max)
  (define (loop c1 c2 cd)
    (if (eq? (+ c1 c2 cd) max)
        (print name1 ":" c1 " " name2 ":" c2 " draw:" cd)
        (let ((result (fight name1 name2 n)))
          (cond ((eq? result name1) (loop (+ c1 1) c2 cd))
                ((eq? result name2) (loop c1 (+ c2 1) cd))
                (else (loop c1 c2 (+ cd 1)))))))
  (loop 0 0 0))

;;実行
(run 'P1 'P2 3 10000)
