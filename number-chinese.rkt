;number-chinese.rkt
;阿拉伯数字与中文的转换。
#lang racket

(provide
 (contract-out
  [number->chinese (->* (integer?)
                        (#:style number-chinese-symbol?)
                        string?)]))

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
(define-syntax-rule (shi-id) 10);十
(define-syntax-rule (bai-id) 11);百
(define-syntax-rule (qian-id) 12);千
(define-syntax-rule (wan-id) 13);万
(define-syntax-rule (yi-id) 14);亿
(define-syntax-rule (zhao-id) 15);兆

;定义节长度：
(define-syntax-rule (qian-len) 4);千
(define-syntax-rule (wan-len) 8);万
(define-syntax-rule (yi-len) 16);亿
;--------------------------------------------------------
;将数字转化为中文字串：
(define (number->chinese num #:style [tag 'normal])
  ;根据参数情况设置全局chinese-vector值：
  (set! chinese-vector
        (cond
          [(equal? tag 'normal) chinese-number]
          [(equal? tag 'capitalization) chinese-t-number]
          [else (display "#:style参数错误。")]))
  (if (= num 0) ;检测是否为0。
      (vector-ref chinese-vector 0);零
      (parse-number-section (number->string num))))

;解析千分节数：
(define (parse-number-section num-str)
  (if (<= (string-length num-str) (qian-len));千级以内的数
      (parse-section-qian num-str)
      (let [(qian-rest-len (- (string-length num-str) (qian-len)))]
        (string-append
         (parse-section-wan (substring num-str 0 qian-rest-len))
         (vector-ref chinese-vector (wan-id));万
         (parse-section-qian (substring num-str qian-rest-len))))))

;解析万分节数：
(define (parse-section-wan num-str)
  (if (<= (string-length num-str) (qian-len));千级以内的数
      (parse-number-section num-str)
      (let [(wan-rest-len (- (string-length num-str) (qian-len)))]
        (string-append (parse-section-yi
                        (substring num-str 0 wan-rest-len))
                       (vector-ref chinese-vector (yi-id));亿
                       (parse-section-wan
                        (substring num-str wan-rest-len))))))

;解析亿分节数：
(define (parse-section-yi num-str)
  (if (<= (string-length num-str) (wan-len));万级以内的数
      (parse-number-section num-str)
      (let [(yi-rest-len (- (string-length num-str) (wan-len)))]
        (string-append
         (parse-section-zhao (substring num-str 0 yi-rest-len))
         (vector-ref chinese-vector (zhao-id));兆
         (parse-section-yi (substring num-str yi-rest-len))))))

;解析兆分节数：
;兆分节数会形成反复出现。
(define (parse-section-zhao num-str)
   (if (<= (string-length num-str) (yi-len));亿级以内的数
       (parse-number-section num-str) ;重复单个亿级数据转化
       (let [(zao-rest-len (- (string-length num-str) (yi-len)))]
         (string-append
          (parse-section-zhao (substring num-str 0 zao-rest-len))
          (vector-ref chinese-vector (zhao-id));兆
          (parse-section-zhao (substring num-str zao-rest-len))))))

;解析一个千以内的数字字串的片段：
(define (parse-section-qian num-str)
  (cond
    [(= (string->number num-str) 0) ""] ;整段长度为零，转化为空值。
    [(= (string-length num-str) 1) ;为最后一个字符，在后边加上量词。
     (generate-num-chinese (substring num-str 0 1)
                           (string-length num-str))]
    [else ;递归解析整段字串片段：
     (let [(cur-ch-str ;当前中文数字。
            (generate-num-chinese (substring num-str 0 1)
                                  (string-length num-str)))
           (rest-ch-str ;剩下的中文片段。
            (parse-number-section (substring num-str 1)))
           ;已经生成的中文数字串最后一个字符（用于检查是否有连续的零）：
           (zero-str (vector-ref chinese-vector 0))]
       ;检查当前得到的字符是否和后边的第一个字符都为零（合并多个相邻的零）：
       (if (and (equal? (substring cur-ch-str 0 1) zero-str)
                (equal? (substring rest-ch-str 0 1) zero-str))
           rest-ch-str
           (string-append cur-ch-str rest-ch-str)))]))

;生成单个数字的中文带量词：
;num-char单个数字，len单个数字对应的数字串位置。
(define (generate-num-chinese num-char len)
  (if (= (string->number num-char) 0)
      (vector-ref chinese-vector 0)
      (string-append (convert-num-chinese num-char)
                     (convert-level-chinese len))))

;将单个数字转化为中文：
(define (convert-num-chinese num-char)
  (vector-ref chinese-vector
              (string->number num-char)))

;根据数字位数生成量词（千以内）：
(define (convert-level-chinese len)
  (cond
    [(= (string-length "1") len) ""]
    [(= (string-length "10") len)
     (vector-ref chinese-vector (shi-id))]
    [(= (string-length "100") len)
     (vector-ref chinese-vector (bai-id))]
    [(= (string-length "1000") len)
     (vector-ref chinese-vector (qian-id))]))
