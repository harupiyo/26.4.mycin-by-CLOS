compile-file
http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp_fi.htm
    => コンパイル済みファイルの名前、警告-p、失敗-p の３値を返す

1. compile-file で警告、エラーのチェック
ql:quickload ではスルーされてしまう警告の中にたくさんのミスが見つかるから、
最初はcompile-file を手動で起動するのがいいとわかった。

(compile-file "utilities")
(compile-file "certainly-factors")
(compile-file "store")
(compile-file "parameter")

    これはコンパイラではなく、たまたま気が付いた！
        (defun get-parameter-from-store (parameter-name)
          "Look up the parameter structure with this name."
          (or (get-store parameter-name *store*)
              (defparameter&store parameter-name))) <--- TODO おっと、ここ、変数展開されないぞ！

    オリジナルのソースではどうやっているか調べたら、defparm と同じことを手動でやっていた！
    https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L146
    https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L138

    修正
        (defun get-parameter-from-store (parameter-name)
          "Look up the parameter structure with this name."
          (or (get-store parameter-name *store*)
              ;; 次行、
              ;; (put-store parameter-name (make-instance 'parameter :name parameter-name) *store*) 
              ;; は、
              ;; (defparameter&store parameter-name) <--- おっと、ここ、変数展開されないぞ！ 
              ;; を展開したもので、マクロに変数を渡せないことによる。
              ;;
              ;; オリジナルのソースでも同様のことをしている
              ;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L146
              ;; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L138
              (put-store parameter-name (make-instance 'parameter :name parameter-name) *store*))) 

(compile-file "parameter-with-value-cf")
(compile-file "context")
(compile-file "rule")

ここまで、WARNING を拾いまくる。ミスがあるよ～！コンパイラはかなりミスを見つけてくれて神！

(compile-file "expert-system")

; file: /home/harupiyo/common-lisp/my-mycin/expert-system.lisp
; in: DEFUN REPORT-FINDINGS
;     (CONTEXT:CONTEXT-NAME PARAMETER:CONTEXT)
; 
; caught STYLE-WARNING:
;   undefined function: CONTEXT:CONTEXT-NAME

    -> context-name はマクロで生成しているコードにあるのでこれは無視していい

; caught STYLE-WARNING:
;   undefined function: RULE:GET-RULES

    -> GET-RULES はトップレベルで定義していないからこれは無視していい
        ; https://github.com/cl-aip/mycin/blob/master/mycin.lisp#L175
        (let ((rules (make-hash-table)))

          (defun get-rules (parameter-name)
            ...))

; caught STYLE-WARNING:
;   undefined function: EXPERT-SYSTEM::REST2

    -> おっと！:use しわすれていた。追加
        (defpackage :expert-system
          (:use ... :utilities) <-- 追加
          ...)


(compile-file "mycin")
; in: DEFUN EMYCIN
;     (CLEAR-STORE)
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::CLEAR-STORE

    -> おかしいな、STORE::CLEAR-STORE をエクスポートしているはずなんだけどな
        get-context-data も同様のエラーを出してきた。仕方なくこうしたらOK にたった
          (store:clear-store)
          (expert-system:get-context-data contexts) 
        パッケージ名で修飾しているのは行mycin.lisp だけだ
            ほかは大丈夫そうなのにな

あと、micyn.lisp ではマクロdefcontext のところでWARNING が出まくる。PATIENT などをシンボルとして読み込んで、そんなシンボルは無い、というたぐい。
    (list (defcontext PATIENT  (name sex age)  ())
          (defcontext CULTURE  (site days-old) ())
          (defcontext ORGANISM ()              (identity)))  

さて、どうしたらいいかな？

結局、mycin.lisp のコンパイル結果は
    (compile-file "mycin")
    ; compilation unit finished
    ;   Undefined functions:
    ;     DEFCONTEXT NAME SITE
    ;   Undefined variables:
    ;     AGE CULTURE DAYS-OLD ORGANISM PATIENT SEX
    ;   caught 7 WARNING conditions
    ;   caught 3 STYLE-WARNING conditions

    ; wrote /home/harupiyo/common-lisp/my-mycin/mycin.fasl
    ; compilation finished in 0:00:00.008
    #P"/home/harupiyo/common-lisp/my-mycin/mycin.fasl"
    T
    T <-- エラーありと判定されてしまっている

    あ！context.lisp でdefcontext をエクスポートしわすれていた -> 修正

    それでもdefcontext が無いというので、パッケージ名修飾をしたらコンパイルに通るようになった！
        (list (context:defcontext PATIENT  (name sex age)  ())
              (context:defcontext CULTURE  (site days-old) ())
              (context:defcontext ORGANISM ()              (identity)))  

    パッケージ名修飾について、予めload しておくとでなくなる。
    compile-file しただけではだめ。
    compile-file はロードもするはずだが、おかしいな。

(compile-file "mycin-r")  

    最初mycin-r.lisp にもdefpackage mycin の定義を置いて、mycin-r で必要なパッケージを指定していたのだが、
    mycin.lisp がわにあるdefpackage と衝突するということで、mycin.lisp のdefpackage にmycin-r で必要なパッケージを追加して
    mycin-r.lisp では in-package :mycin するだけにした。

; file: /home/harupiyo/common-lisp/my-mycin/mycin-r.lisp
; in: DEFRULE 165
;     (RULE:DEFRULE 165
;       IF
;       (MYCIN::GRAM MYCIN::ORGANISM MYCIN::IS MYCIN::POS)
;       (MYCIN::MORPHOLOGY MYCIN::ORGANISM MYCIN::IS MYCIN::COCCUS)
;       (MYCIN::GROWTH-CONFORMATION MYCIN::ORGANISM MYCIN::IS MYCIN::CHAINS)
;       MYCIN::THEN
;       0.7
;       (IDENTITY MYCIN::ORGANISM MYCIN::IS MYCIN::STREPTOCOCCUS))
; 
; caught WARNING:
;   Rule 165: Illegal certainty factor: NIL
    この警告文は何を言っているのかと思ったが、決定要因(Certainly factor. Common Lisp ではなくMYCIN の用語) と言っているから、rule.lisp 側でプログラムからエラーを出しているのだと気づいた。

# 謎

rule.lisp 上で
    (defrule 165
      if (gram ORGANISM is pos)
         (morphology ORGANISM is coccus)
         (growth-conformation ORGANISM is chains)
      then .7
         (identity ORGANISM is streptococcus))
しても出ない問題が、
mycin-r.lisp 上では出てしまう。

    (defmacro defrule (number &body body)
      (assert (eq (first body) 'if))
      (let* ((then-part (member 'then body))  <---- member が効いていないようで、
             (premises (ldiff (rest body) then-part))
             (conclusions (rest2 then-part))
             (cf (second then-part)))
        ;; [TODO] 後回し Do some error checking:
        ; (check-conditions number premises 'premise)
        ; (check-conditions number conclusions 'conclusion)
        (format t "DEBUG(in defrule): then:~A ~%cf:~A" then-part cf)
        (when (not (cf-p cf))
          (error "Rule ~a: Illegal certainty factor: ~a" number cf)  <--- ここで捕まる
        ;; Now build the rule:
        `(put-rule
             (make-instance 'rule :number ,number :cf ,cf :premises ',premises :conclusions ',conclusions)))))

    この問題の再現手順
        (compile-file "utilities")
        (compile-file "certainly-factors")
        (compile-file "store")
        (compile-file "parameter")
        (compile-file "parameter-with-value-cf")
        (compile-file "context")
        (compile-file "rule")
        (compile-file "expert-system")
        (compile-file "mycin")
        (compile-file "mycin-r")   => error

        mycin-r.lisp 上でdefrule を評価するために必要なファイルを読み込み
        (load "utilities")
        (load "certainly-factors")
        (load "store")
        (load "parameter")
        (load "parameter-with-value-cf")
        (load "context")
        (load "rule")

        mycin-r.lisp 上で
            defrule を評価するとエラー
        rule.lisp 上で
            defrule を評価してもエラーにならない
        mycin-r.lisp 上に
            defrule の定義を持ってきて評価し、
            defrule を評価すると通る
        ... 不思議だ。

        1. load がなぜ必要か？
        2. rule.lisp とmycin-r.lisp で評価するとは何が違うのか？
    
2021/11/16 現在この問題のために先に進めていない。特にエンジン部分、expert-sytem.lisp はまだ何もテストできていない。続きはここから。

--------------------------------------------------------------------------------

このようなcompile-file ならではの問題に初めて出会ったな！この体験は非常に良い
Lisp はファイルを読み直しても以前の間違った定義がのこることがあるので、失敗したらSBCL 再起動して評価し直し

(compile-file "utilities")
(compile-file "certainly-factors")
(compile-file "store")
(compile-file "parameter")
(compile-file "parameter-with-value-cf")
(compile-file "context")
(compile-file "rule")
(compile-file "expert-system")
(compile-file "mycin")
(compile-file "mycin-r")) 

(load "utilities")
(load "certainly-factors")
(load "store")
(load "parameter")
(load "parameter-with-value-cf")
(load "context")
(load "rule")

;; 開発中に、全ソースを再コンパイルが必要なときに
(asdf:compile-system :expert-system :force t)
;; 完成した暁にはこれでロード
(ql:quickload :expert-system)
