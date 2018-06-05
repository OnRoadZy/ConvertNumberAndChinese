;chinese-number.rkt
;阿拉伯数字与中文的转换。
#lang racket

(provide
 (contract-out
  [chinese->number (->* (string?)
                        (#:style number-chinese-symbol?)
                        integer?)]))

;定义number-chinese-symbol?合约：
(define (number-chinese-symbol? flag)
  (if (symbol? flag)
      (case flag
        [('normal 'capitalization) flag]
        [else '参数值错误。])
      '不是symbol类型。))

;定义数据列表：
(define chinese-number (vector "零" "一" "二" "三" "四" "五" "六" "七" "八" "九" "十" "百" "千" "万" "亿" "兆"))
(define chinese-t-number (vector "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖" "拾" "佰" "仟" "万" "亿" "兆"))
(define number-vector (vector 0 1 2 3 4 5 6 7 8 9 10 100 1000 10000 100000000 10000000000000000))

;初始化全局chinese-vector：
(define chinese-vector chinese-number) 

;定义量级对应列表位置宏：
(define-syntax-rule (wan-id) 13);万
(define-syntax-rule (yi-id) 14);亿
(define-syntax-rule (zhao-id) 15);万
;-----------------------------------------
;将中文字串转化为数字：
(define (chinese->number ch-str #:style [tag 'normal])
  ;根据参数情况设置全局chinese-vector值：
  (set! chinese-vector
        (cond
          [(equal? tag 'normal) chinese-number]
          [(equal? tag 'capitalization) chinese-t-number]
          [else (display "#:style参数错误。")]))
  (if (equal? ch-str "")
      0
      (parse-chinese-section ch-str "")))

;对中文字串千分节解析：
;从低位到高位顺序对中文字串解析求值：
(define (parse-chinese-section ch-str section-str)
  (if (= (string-length ch-str) 1)
      ;进行千小节解析计算:
      (parse-section-qian (string-append ch-str section-str))
      ;顺序进行字串解析：
      (let* [(len (string-length ch-str));字串长度。
             (ch (substring ch-str (- len 1)))];提取字串最后一个字符。
        (cond
          [(equal? ch (vector-ref chinese-vector (wan-id)));万
           (+ (parse-section-qian section-str)
              (parse-section-wan
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [(equal? ch (vector-ref chinese-vector (yi-id)));亿
           (+ (parse-section-qian section-str)
              (parse-section-yi
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [(equal? ch (vector-ref chinese-vector (zhao-id)));兆
           (+ (parse-section-qian section-str)
              (parse-section-zhao
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [else
           (parse-chinese-section
            (substring ch-str 0 (- len 1))
            (string-append ch section-str))]))))

;对中文字串万分节解析：
(define (parse-section-wan ch-str section-str)
  (if (= (string-length ch-str) 1)
      ;进行千分节解析:
      (* (vector-ref number-vector (wan-id)) ;万对应的数量级。
         (parse-chinese-section
          (string-append ch-str section-str) ""))
      ;顺序进行字串解析：
      (let* [(len (string-length ch-str));字串长度。
             (ch (substring ch-str (- len 1)))];提取字串最后一个字符。
        (cond
          [(equal? ch (vector-ref chinese-vector (yi-id)));亿
           (+ (parse-section-wan section-str "")
              (parse-section-yi
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [(equal? ch (vector-ref chinese-vector (zhao-id)));兆
           (+ (parse-section-wan section-str "")
              (parse-section-zhao
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [else
           (parse-section-wan
            (substring ch-str 0 (- len 1))
            (string-append ch section-str))]))))

;对中文字串亿分节解析：
(define (parse-section-yi ch-str section-str)
  (if (= (string-length ch-str) 1)
      ;进行亿分节解析：
      (* (vector-ref number-vector (yi-id)) ;亿对应的数量级。
         (parse-chinese-section
          (string-append ch-str section-str) ""))
      ;顺序进行字串解析：
      (let* [(len (string-length ch-str));字串长度。
             (ch (substring ch-str (- len 1)))];提取字串最后一个字符。
        (cond
          [(equal? ch (vector-ref chinese-vector (zhao-id)));兆
           (+ (parse-section-yi section-str "")
              (parse-section-zhao
               (substring ch-str 0 (- len 1))
               ""))] ;分节字串清空。
          [else
           (parse-section-yi
            (substring ch-str 0 (- len 1))
            (string-append ch section-str))]))))

;对中文字串兆分节解析：
(define (parse-section-zhao ch-str section-str [zhao-times 1])
  (if (= (string-length ch-str) 1)
      ;进行兆分节解析:
      (* (expt (vector-ref number-vector (zhao-id)) ;兆对应的数量级。
               zhao-times)
         (parse-chinese-section
          (string-append ch-str section-str) ""))
      ;顺序进行字串解析：
      (let* [(len (string-length ch-str));字串长度。
             (ch (substring ch-str (- len 1)))];提取字串最后一个字符。
        (cond
          [(equal? ch (vector-ref chinese-vector (zhao-id)));兆
           (+ (parse-section-zhao section-str "" zhao-times)
              (parse-section-zhao
               (substring ch-str 0 (- len 1))
               "" ;分节字串清空。
               (+ zhao-times 1)))]
          [else
           (parse-section-zhao
            (substring ch-str 0 (- len 1))
            (string-append ch section-str)
            zhao-times)]))))

;解析千以内分节解析并求值：
(define (parse-section-qian ch-str)
  (cond
    [(equal? ch-str "") 0]
    [(= (string-length ch-str) 1)
     ;仅有一个值，必然是数字，直接返回该对应值：
     (vector-ref number-vector
                 (vector-member ch-str chinese-vector))]
      ;取得相邻两个数子单字串，
      ;非特殊情况，应为数值和量词：
    [else
     (let* [;取得字串长度：
            (len (string-length ch-str))
            ;取得数值（先当做是数值）：
            (num (vector-ref
                   number-vector
                   (vector-member
                    (substring ch-str (- len 2) (- len 1))
                    chinese-vector)))
            ;取得量词的位置：
            (unit-pos (vector-member
                       (substring ch-str (- len 1) len)
                       chinese-vector))
            ;取得量词对应的两级数字：
            (level (vector-ref number-vector unit-pos))]
       (cond
         ;最后2个字符，必然是数字和量词组合：
         [(= len 2) (* level num)]
         ;原定量词位置为数字,则忽略量词直接作为数字加上:
         [(and (>= unit-pos 0) (<= unit-pos 9))
          (+ level
             (parse-section-qian
              (substring ch-str 0 (- len 1))))]
         ;通常情况，数字和量词组合：
         [else
          (+ (* level num)
             (parse-section-qian
              (substring ch-str 0 (- len 2))))]))]))
