#lang racket
(require "number-chinese.rkt"
         "chinese-number.rkt")

(define ch-test-list
  '(("零" "二千零三" "一百二十三" "二千零一十三" "一千二百" "二千七百一十三" "一万二千三百四十五" "一百二十三万四千零六十九" "九亿三千万" "九亿三千一百二十三万四千零六十九" "一千二百三十四兆五千零六万七千八百九十亿零一百二十三万四千零六十九" "五十八兆四千三百六十万二千八百九十亿零二百四十八万一千二百三十四兆五千零六万七千八百九十亿零一百二十三万四千零六十九")
    ("零" "贰仟零叁" "壹佰贰拾叁" "贰仟零壹拾叁" "壹仟贰佰" "贰仟柒佰壹拾叁" "壹万贰仟叁佰肆拾伍" "壹佰贰拾叁万肆仟零陆拾玖" "玖亿叁仟万" "玖亿叁仟壹佰贰拾叁万肆仟零陆拾玖" "壹仟贰佰叁拾肆兆伍仟零陆万柒仟捌佰玖拾亿零壹佰贰拾叁万肆仟零陆拾玖" "伍拾捌兆肆仟叁佰陆拾万贰仟捌佰玖拾亿零贰佰肆拾捌万壹仟贰佰叁拾肆兆伍仟零陆万柒仟捌佰玖拾亿零壹佰贰拾叁万肆仟零陆拾玖")
    (0 2003 123 2013 1200 2713 12345 1234069 930000000 931234069 12345006789001234069 5843602890024812345006789001234069)))

;测试数字转中文：
(for/list [(num (list-ref ch-test-list 2))
           (ch (list-ref ch-test-list 0))]
  (if (equal? (number->chinese num) ch)
      (display (format "~a->~a\n" num ch))
      (display (format "错误！~a预期生成~a,实际生成~a\n"
                       num ch (number->chinese num)))))

(for/list [(num (list-ref ch-test-list 2))
           (ch-t (list-ref ch-test-list 1))]
  (if (equal? (number->chinese num #:style 'capitalization) ch-t)
      (display (format "~a->~a\n" num ch-t))
      (display (format "错误！~a预期生成~a,实际生成~a\n"
                       num ch-t (number->chinese num #:style 'capitalization)))))
  
;测试中文转数字：
(for/list [(ch (list-ref ch-test-list 0))
           (num (list-ref ch-test-list 2))]
  (if (= (chinese->number ch) num)
      (display (format "~a->~a\n" ch num))
      (display (format "错误！~a预期结果为~a，实际结果为~a。\n"
                       ch num (chinese->number ch)))))
  
(for/list [(ch-t (list-ref ch-test-list 1))
           (num (list-ref ch-test-list 2))]
  (if (= (chinese->number ch-t #:style 'capitalization) num)
      (display (format "~a->~a\n" ch-t num))
      (display (format "错误！~a预期结果为~a，实际结果为~a。\n"
                       ch-t num (chinese->number ch-t #:style 'capitalization)))))