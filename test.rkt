#lang racket
(require "number-chinese.rkt"
         "chinese-number.rkt")

(define ch-test-list
  '(("零" "一" "二" "三" "四" "五" "六" "七" "八" "九" "一十" "一十一" "一百一十" "一百一十一" "一百" "一百零二" "一千零二十" "一千零一" "一千零一十五" "一千" "一万" "二万零一十" "二万零一" "一十万" "一百万" "一千万" "一亿" "一十亿" "一十亿一千" "一十亿零一百" "二十万零一十" "二百万零一百零五" "二千万一千零七" "二十亿零一十万零一百九十" "一十亿四千零一万" "二亿零一万二千三百零一" "二十亿零五百零一万零一十" "四十亿零九百零六万零二百" "四十二亿九千四百九十六万七千二百九十五" "一千二百三十四兆五千零六万七千八百九十亿零一百二十三万四千零六十九" "五十八兆四千三百六十万二千八百九十亿零二百四十八万一千二百三十四兆五千零六万七千八百九十亿零一百二十三万四千零六十九")
    ("零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖" "壹拾" "壹拾壹" "壹佰壹拾" "壹佰壹拾壹" "壹佰" "壹佰零贰" "壹仟零贰拾" "壹仟零壹" "壹仟零壹拾伍" "壹仟" "壹万" "贰万零壹拾" "贰万零壹" "壹拾万" "壹佰万" "壹仟万" "壹亿" "壹拾亿" "壹拾亿壹仟" "壹拾亿零壹佰" "贰拾万零壹拾" "贰佰万零壹佰零伍" "贰仟万壹仟零柒" "贰拾亿零壹拾万零壹佰玖拾" "壹拾亿肆仟零壹万" "贰亿零壹万贰仟叁佰零壹" "贰拾亿零伍佰零壹万零壹拾" "肆拾亿零玖佰零陆万零贰佰" "肆拾贰亿玖仟肆佰玖拾陆万柒仟贰佰玖拾伍" "壹仟贰佰叁拾肆兆伍仟零陆万柒仟捌佰玖拾亿零壹佰贰拾叁万肆仟零陆拾玖" "伍拾捌兆肆仟叁佰陆拾万贰仟捌佰玖拾亿零贰佰肆拾捌万壹仟贰佰叁拾肆兆伍仟零陆万柒仟捌佰玖拾亿零壹佰贰拾叁万肆仟零陆拾玖")
    (0 1 2 3 4 5 6 7 8 9 10 11 110 111 100 102 1020 1001 1015 1000 10000 20010 20001 100000 1000000 10000000 100000000 1000000000 1000001000 1000000100 200010 2000105 20001007 2000100190 1040010000 200012301 2005010010 4009060200 4294967295 12345006789001234069 5843602890024812345006789001234069)))

;测试数字转中文：
(display ;打印简体转换。
 (for/list [(num (list-ref ch-test-list 2))
            (ch (list-ref ch-test-list 0))]
   (if (equal? (number->chinese num) ch)
       (format "~a->~a\n" num ch)
       (format "错误！~a预期生成~a,实际生成~a\n"
               num ch (number->chinese num)))))
(display ;打印大写转换。
 (for/list [(num (list-ref ch-test-list 2))
            (ch-t (list-ref ch-test-list 1))]
   (if (equal? (number->chinese num #:style 'capitalization) ch-t)
       (format "~a->~a\n" num ch-t)
       (format "错误！~a预期生成~a,实际生成~a\n"
               num ch-t (number->chinese num #:style 'capitalization)))))
  
;测试中文转数字：
(display ;打印简体转换。
 (for/list [(ch (list-ref ch-test-list 0))
            (num (list-ref ch-test-list 2))]
   (if (= (chinese->number ch) num)
       (format "~a->~a\n" ch num)
       (format "错误！~a预期结果为~a，实际结果为~a。\n"
               ch num (chinese->number ch)))))
(display ;打印大写转换。
 (for/list [(ch-t (list-ref ch-test-list 1))
           (num (list-ref ch-test-list 2))]
  (if (= (chinese->number ch-t #:style 'capitalization) num)
      (format "~a->~a\n" ch-t num)
      (format "错误！~a预期结果为~a，实际结果为~a。\n"
                       ch-t num (chinese->number ch-t #:style 'capitalization)))))