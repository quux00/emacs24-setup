;; ----------------------------------------------------- ;;
;; ------------------- emacs macros -------------------- ;;
;; ----------------------------------------------------- ;;

;; Note: how to create, name and save an emacs macro
;;   Create by: C-x ( <do macro steps here> C-x )
;;   Name it with: C-x C-k n (kmacro-name-last-macro)
;;   Open file you want to save it in and type: M-x insert-kbd-macro <RET> macroname <RET>

;; ---- general macros ---- ;
;; this will uncommont the current line you are on and only the current line
;; do not set mark and point for this, just invoke for a single give line
(fset 'uc
   [?\C-a ?\C-  down ?\C-u ?\C-c ?\C-c up])

;; "highlight comments" for C-based comment languages
;; produces: /* ---[  ]--- */
(fset 'hic
   [tab ?/ ?* ?  ?- ?- ?- ?\[ ?  ?  ?\] ?- ?- ?- ?  ?* ?/ left left left left left left left left])

;; "highlight comments" for Ruby-like comment languages
;; produces: # ---[  ]--- #
(fset 'ric
   [tab ?# ?  ?- ?- ?- ?\[ ?  ?  ?\] ?- ?- ?- ?  ?# left left left left left left left])

;; ---[ blog macros ]--- ;;

;; inserts <pre> tags with call to syntaxhighlighter (default is Java)
(fset 'blogcode
   [?< ?p ?r ?e ?  ?c ?l ?a ?s ?s ?= ?\" ?b ?r ?u ?s ?h ?: ?  ?j ?a ?v ?a ?\" ?> return return ?< ?/ ?p ?r ?e ?> up])

(fset 'blog-hdr
   [?# ?# ?# ?# ?  ?` ?/ ?* ?  ?- ?- ?- ?\[ ?  ?  right ?- ?- ?- ?  ?* ?/ ?` left left left left left left left left left])


;; ---- html macros ---- ;
(fset 'html-jq-tmpl
   [?< ?h ?t ?m ?l ?> return ?< ?h ?e ?a ?d ?> return ?< ?t ?i ?t ?l ?e ?> ?< ?/ ?t ?i ?t ?l ?e ?> return return return ?< ?/ ?h ?e ?a ?d ?> tab return ?< ?b ?o ?d ?y ?> return return return ?< ?s ?c ?r ?i ?p ?t ?  ?s ?r ?c ?= ?\" ?\" ?> ?< ?/ ?s ?c ?r ?i ?p ?t ?> left left left left left left left left left left left ?l ?i ?b left left left ?. ?. ?/ right right right ?/ ?j ?q ?u ?e ?r ?y ?- ?1 ?. ?7 ?. ?1 ?. ?j ?s ?\C-e return ?< ?/ ?b ?o ?d ?y ?> tab return ?< ?/ ?h ?t ?m ?l ?> tab up up up up up up up up up right right right right])

(fset 'dt
   "<!doctype html>\C-e")

(fset 'html-tmpl
   [?< ?! ?d ?o ?c ?t ?y ?p ?e ?  ?h ?t ?m ?l ?> return ?< ?h ?t ?m ?l ?> return ?< ?h ?e ?a ?d ?> return ?< ?m ?e ?t ?a ?  ?c ?h ?a ?r ?s ?e ?t ?= ?\" ?U ?t backspace ?T ?F ?- ?8 ?\" ?> return ?< ?l ?i ?n ?k ?  ?r ?e ?l ?= ?\" ?s ?t ?y ?l ?e ?s ?h ?e ?e ?t ?\" ?  ?h ?r ?e ?f ?= ?\" ?\" ?> return ?< ?t ?i ?t ?l ?e ?> ?< ?/ ?t ?i ?t ?l ?e ?> return ?< ?/ ?h ?e ?a ?d ?> tab return ?< ?b ?o ?d ?y ?> return return ?< ?s ?c ?r ?i ?p ?t ?  ?s ?r ?c ?= ?\" ?\" ?> ?< ?/ ?s ?c ?r ?i ?p ?t ?> return ?< ?/ ?b ?o ?d ?y ?> tab return ?< ?/ ?h ?t ?m ?l ?> tab return])



;; ---- xml macros ---- ;
(fset 'xcom
   [tab ?< ?! ?- ?- ?  ?  ?- ?- ?> left left left left])

;; ---- Makefile macros ---- ;
(fset 'mktmpl
   [?C ?C ?= ?t backspace ?g ?c ?c return ?C ?F ?L ?A ?G ?S ?= ?- ?W ?a ?l ?l ?  ?- ?p ?e ?d ?a ?n ?t ?i ?c ?  ?- ?s ?t ?d ?= ?c ?9 ?9 return return return ?h ?e ?l ?p ?: return tab ?@ ?e ?c ?h ?o ?  ?\" ?T ?a ?r ?g ?e ?t ?s ?\" return tab ?@ ?e ?g ?r ?e ?p ?  ?\" ?^ ?\( ?\\ ?- ?| ?\\ ?w ?| ?_ ?\) ?+ ?: ?\" ?  ?\[ ?M ?m ?\} ?a backspace backspace ?\] ?a ?k ?e ?f ?i ?l ?e return return ?c ?l ?e ?a ?n ?: return tab ?@ ?r ?m ?  ?- ?f ?  ?* ?. ?o return tab ?@ ?f ?i ?l ?e ?  ?* ?  ?| ?  ?g ?r ?e ?p ?  ?E ?L ?F ?  ?| ?  ?c ?u ?t ?  ?\" ?- ?d ?: ?\" ?  ?- ?f ?1 ?  ?| ?  ?x ?a ?r ?g ?s ?  ?r ?m ?  ?2 ?> ?/ ?d ?e ?v ?/ ?n ?u ?l ?l ?\; ?  ?l ?s ?  ?> ?/ ?d ?e ?v ?/ ?n ?u ?l ?l ?\; return up up up up up up up up up return])


;; ---- ruby macros --- ;
(fset 'rdd
   [tab ?# ?D ?E ?B ?U ?G ?\C-j ?p ?u ?t ?s ?\C-j ?# ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up ? ])

(fset 'rmain
   [?# ?! ?/ ?u ?s ?r ?/ ?b ?i ?n ?/ ?e ?n ?v ?  ?r ?u ?b ?y return return ?r ?e ?q ?u ?i ?r ?e ?  ?\' ?r ?u ?b ?y ?g ?e ?m ?s ?\' return ?r ?e ?q ?u ?i ?r ?e ?  ?\' ?o ?p ?t ?p ?a ?r ?s ?e ?\' return return ?d ?e ?f ?  ?f return return backspace backspace ?e ?n ?d return return ?d ?e ?f ?  ?m ?a ?i ?n return return backspace backspace ?e ?n ?d return return ?i ?f ?  ?$ ?0 ?  ?= ?= ?  ?_ ?_ ?F ?I ?L ?E ?_ ?_ return ?m ?a ?i ?n return backspace backspace ?e ?n ?d up up up up up up up up up up up up right right right right right right])

(fset 'rcom
   [tab ?# ?  ?D ?e ?s ?c return tab ?# return tab ?# return ?# left tab right up up ?  down ?  down ?  ?@ ?r ?e ?t ?u ?r ?n ?  ?\[ ?A ?r ?r ?a ?y ?< ?S ?t ?r ?i ?n ?g ?> ?\] ?  ?r ?e ?t ?u ?r ?n ?s ?  ?x ?x ?x up ?@ ?p ?a ?r ?a ?m ?  ?\[ ?N ?u ?m ?b ?e ?r ?\] ?  ?n ?u ?m ?  ?s ?i ?z ?e up up left left left left])

(fset 'private
   [tab ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# return tab ?p ?r ?i ?v ?a ?t ?e ?  ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# return tab ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?# ?\C-j])

;; describe for rspec
(fset 'rde
   [tab ?d ?e ?s ?c ?r ?i ?b ?e ?  ?\" ?\" ?  ?d ?o return tab ?i ?t ?  ?\" ?s ?h ?o ?u ?l ?d ?  ?\" ?  ?d ?o return tab ?e ?n ?d tab return tab ?e ?n ?d tab up up up right right right right right right right])

;; template for OptionParser command line option parsing
(fset 'ropts
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 111 112 116 112 97 114 115 101 32 61 32 79 112 116 105 111 110 97 backspace 80 97 114 115 101 114 46 110 101 119 32 32 100 111 32 124 124 left 111 112 116 115 right return return 101 110 100 tab up tab 111 112 116 115 46 98 97 110 110 101 114 32 61 32 34 85 115 97 103 101 58 32 109 46 114 98 32 91 79 80 84 73 79 78 83 93 32 102 105 108 101 92 110 34 return return tab 35 32 100 101 102 105 110 101 32 116 104 101 32 111 112 116 105 111 110 115 return tab 111 112 116 105 111 110 115 91 58 118 101 114 98 111 115 101 93 32 61 32 102 97 108 115 101 return tab 111 112 116 105 backspace 115 46 111 110 57 39 118 backspace backspace backspace 40 39 118 39 left left 45 right right 44 32 32 backspace 39 45 45 118 101 114 98 111 115 101 39 44 32 39 79 117 116 112 117 116 32 109 111 114 101 32 105 110 102 111 109 114 97 116 105 111 110 backspace backspace backspace backspace backspace backspace backspace 114 109 97 116 105 111 110 39 41 32 100 111 return tab 111 112 116 105 111 110 115 91 58 118 101 114 98 111 115 101 93 32 61 32 116 114 117 101 return tab 101 110 100 tab return return tab 111 112 116 105 111 110 115 91 58 118 97 108 93 32 61 32 110 105 108 return tab 111 112 116 115 46 111 110 40 39 45 111 39 44 32 39 45 45 111 115 32 79 83 39 44 32 39 83 112 101 99 105 102 121 32 88 88 88 39 41 32 100 111 32 124 120 124 return return 101 110 100 tab up tab 111 112 116 105 111 110 115 91 58 118 97 108 93 32 61 32 120 down return return tab 35 32 104 101 108 112 32 115 99 114 101 101 110 return tab 111 112 116 115 46 111 110 40 39 45 104 39 44 32 39 45 45 104 101 108 112 39 44 32 39 68 105 115 112 108 97 121 32 116 104 105 115 32 115 99 114 101 101 110 39 41 32 100 111 return tab 112 117 116 115 32 111 112 116 115 return tab 101 120 105 116 return tab 101 110 100 tab down 32 32 35 32 101 110 100 32 79 112 116 105 111 110 80 97 114 115 101 114 46 110 101 119 32 98 108 111 99 107 return return tab 35 32 112 97 114 115 101 32 65 82 71 86 32 backspace 44 32 114 101 109 111 118 105 110 103 32 97 110 121 111 112 116 105 111 backspace backspace backspace backspace backspace 32 111 112 116 105 111 110 115 32 97 110 100 32 116 104 101 105 114 32 112 97 114 97 109 115 return tab 111 112 116 112 97 114 115 101 46 112 97 114 115 101 33] 0 "%d")) arg)))

(fset 'rtempfile
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 114 101 113 117 105 114 101 32 39 116 101 109 112 102 105 108 101 39 return tab 116 109 112 102 105 108 101 32 61 32 84 101 109 112 102 105 108 101 46 110 101 119 40 39 109 121 116 109 112 39 41 return tab 116 109 112 102 105 108 101 46 112 97 116 104 return tab 116 109 112 102 105 108 101 32 60 60 32 34 116 101 120 116 34 return tab 116 109 112 102 105 108 101 46 102 108 105 115 104 backspace backspace backspace 117 115 104 return tab 116 109 112 102 105 108 101 46 99 108 111 115 101 return up up up up up up down right right right right right right right right right right right right right right right right right right right right right right right right right right right right right right] 0 "%d")) arg)))


(fset 'pry
   [tab ?# ?D ?E ?B ?U ?G return tab ?b ?i ?n ?d ?i ?n ?g ?. ?p ?r ?y return tab ?# ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G])


;; ---- java macros --- ;
;; adds a space and then puts balancing braces
(fset 'p
   [?  ?\{ return return ?\} up tab])

(fset 'jtry
   [tab ?t ?r ?y ?  ?\{ return return ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ return return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up up up up up tab])

(fset 'sop
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 83 121 115 116 101 109 46 111 117 116 46 112 114 105 110 116 108 110 40 41 59 left left] 0 "%d")) arg)))

(fset 'sep
   [tab ?S ?y ?s ?t ?e ?m ?. ?e ?r ?r ?. ?p ?r ?i ?n ?t ?l ?n ?\( ?\" ?E ?R ?R ?O ?R ?: ?  ?\" ?  ?+ ?\S-  ?e ?. ?t ?o ?S ?t ?r ?i ?n ?g ?\( ?\) ?\) ?\;])

(fset 'jdd
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 47 47 32 68 69 66 85 71 return return tab 47 47 32 69 78 68 33554464 68 69 66 85 71 up tab 83 121 115 116 101 109 46 111 117 116 46 112 114 105 110 116 108 110 40 41 59 left left] 0 "%d")) arg)))

(fset 'jdoc
   [?/ ?* ?* return ?  ?* return ?  ?* return ?  ?* return ?  ?* ?/ up ?  up ?  up ? ])

(fset 'jmain
   [?i ?m ?p ?o ?r ?t ?  ?j ?a ?v ?a ?. ?u ?t ?i ?l ?. ?* ?\; return return ?p ?u ?b ?l ?i ?c ?  ?c ?l ?a ?s ?s ?  ?X ?  ?\{ return return return ?\} up ?  ?p ?u ?b ?l ?i ?c ?  ?s ?t ?a ?t ?i ?c ?  ?v ?o ?i ?d ?  ?m ?a ?i ?n ?\( ?S ?t ?r ?i ?n ?g ?\[ ?\] ?  ?a ?r ?g ?s ?\) ?  ?\{ return return ?\} up up up return return up ?p ?u ?b ?l ?i ?c ?  ?X ?\( ?\) ?  ?\{ return return ?\} up up right right right right right right right right right up up right right])

(fset 'jtry
   [tab ?t ?r ?y ?  ?\{ return return return ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ return return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up up up up up up right up tab])

;; --- Java neo4j macros --- ;;
(fset 'txneo
   [tab ?T ?r ?a ?n ?s ?a ?c ?t ?i ?o ?n ?  ?t ?x ?  ?= ?  ?d ?b ?. ?b ?e ?g ?i ?n ?g backspace ?T ?x ?\( ?\) ?\; ?\C-j ?t ?r ?y ?  ?\{ ?\C-j ?\C-j ?\C-j ?t ?x ?. ?s ?u ?c ?c ?e ?s ?s ?\( ?\) ?\; return tab ?\} ?  ?c ?a ?t ?c ?h ?  ?\( ?E ?x ?c ?e ?p ?t ?i ?o ?n ?  ?e ?\) ?  ?\{ ?\C-j ?e ?. ?p ?r ?i ?n ?t ?S ?t ?a ?c ?k ?T ?r ?a ?c ?e ?\( ?\) ?\; ?\C-j ?t ?h ?r ?o ?w ?  ?n ?e ?w ?  ?R ?u ?n ?t ?i ?m ?e ?E ?x ?c ?e ?p ?t ?i ?o ?n ?\( ?e ?\) ?\; return ?\} ?  ?f ?i ?n ?a ?l ?l ?y ?  ?\{ return return ?\} up tab ?t ?x ?. ?f ?i ?n ?i ?s ?h ?\( ?\) ?\; up up up up up up up tab])

(fset 'cypher
   [tab ?S ?t ?r ?i ?n ?g ?  ?c ?q ?l ?  ?= ?  return ?\" ?S ?T ?A ?R ?T ?  ?  ?\" ?  ?+ return ?\" ?M ?A ?T ?C ?H ?  ?  ?\" ?  ?+ return ?\" ?W ?H ?E ?R ?E ?  ?  ?\" ?  ?+ return ?\" ?R ?E ?T ?U ?R ?N ?  ?\" ?\; return ?E ?x ?e ?c ?u ?t ?i ?o ?n ?R ?e ?s ?u ?l ?t ?  ?r ?t ?  ?= ?  ?e ?n ?g ?i ?n ?e ?. ?e ?x ?e ?c ?u ?t ?e ?\( ?q ?r ?y ?\) ?\; up up up up left left left left])

;; --- Go macros --- ;;
(fset 'gop
   [tab ?f ?m ?t ?. ?P ?r ?i ?n ?t ?f ?\( ?\" ?% ?v ?\\ ?n right ?, ? ])

(fset 'goerr
   [tab ?i ?f ?  ?e ?r ?r ?  ?! ?= ?  ?n ?i ?l ?  ?\{ return return up tab ?f ?m ?t ?. ?F ?p ?r ?i ?n ?t ?f ?\( ?o ?s ?. ?S ?t ?d ?e ?r ?r ?, ?  ?\" ?W ?A ?R ?N ?: ?  ?% ?v ?\\ ?n ?\" ?, ?  ?e ?r ?r ?\C-e return ?r ?e ?t ?u ?r ?n ?  ?e ?r ?r up C-right C-right C-right C-right right right])

(fset 'go-tmpl
   [?p ?a ?c ?k ?a ?g ?e ?  ?m ?a ?i ?n return return ?i ?m ?p ?o ?r ?t ?  ?\( return return up tab ?\" ?m backspace ?f ?m ?t down return return ?f ?u ?n ?c ?  ?m ?a ?i ?n ?\( ?\) ?  ?\{ return return up tab])

(fset 'goregex
   [tab ?r ?x ?  ?: ?= ?  ?r ?e ?g ?e ?x ?p ?. ?M ?u ?s ?t ?C ?o ?m ?p ?i ?l ?e ?\( ?` ?\C-e return ?r ?e ?s ?  ?: ?= ?  ?r ?x ?. ?F ?i ?n ?d ?S ?t ?r ?i ?n ?g ?S ?u ?b ?m ?a ?t ?c ?h ?\( ?s ?\C-e return ?i ?f ?  ?r ?e ?s ?  ?! ?= ?  ?n ?i ?l ?  ?\{ return return tab up tab ?a ?  ?= ?  ?r ?e ?s ?\[ ?1 ?\C-e ?  ?/ ?/ ?  ?$ ?1 up up up right right right right right right])



;; --- Clojure macros --- ;;
(fset 'lic
   [?\; ?\; ?  ?- ?- ?- ?\[ ?  ?  ?\] ?- ?- ?- ?  ?\; ?\; left left left left left left left left])

(fset 'cjdd
   [tab ?\; ?\; ?  ?D ?E ?B ?U ?G return return ?\; ?\; ?  ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G tab up tab ?\( ?p ?r ?i ?n ?t ?l ?n ?  ?\"])


; clojure.test "are" macro
(fset 'are
   [tab ?\( ?a ?r ?e ?  ?\[ ?x ?  ?y right ?  ?\( ?= ?  ?x ?  ?y right ?\C-j])

;; --- Groovy macros --- ;;
(fset 'groovy-tmpl
   [?c ?l ?a ?s ?s ?  ?S ?c ?r ?i ?p ?t ?  ?\{ return return return up tab ?d ?e ?f ?  ?m ?a ?i ?n ?\( ?a ?r ?g ?s right ?  ?\{ return return down return return ?n ?e ?w ?  ?S ?c ?r ?i ?p ?t ?\( ?\) ?. ?m ?a ?i ?n ?\( ?a ?r ?g ?s ?\C-e return up up up up up tab])



;; --- javascript macros --- ;
(fset 'jsdd
   [tab ?/ ?/ ?D ?E ?B ?U ?G ?\C-j ?a ?l ?e ?r ?t ?\( ?\" ?\" ?\) ?\; ?\C-j ?/ ?/ ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up left left left])

(fset 'jss-for
   [tab ?f ?o ?r ?  ?\( ?v ?a ?r ?  ?i ?= ?0 ?\; ?  ?i ?  ?< ?  ?a ?r ?y ?. ?l ?e ?n ?g ?t ?h ?\; ?  ?i ?+ ?+ ?\) ?  ?\{ return return ?\} up up right right right right right right right right right right right right right right right right right right right right])

;; create the shell/outline of a javascript lambda expression
(fset 'jslambda
   [?\( ?\) ?\( ?\) ?\; left left left left ?f ?u ?n ?c ?t ?i ?o ?n ?\( ?\) ?  ?\{ return return ?\} tab up tab])

(fset 'jsfunc
   [?f ?u ?n ?c ?t ?i ?o ?n ?\( ?\) ?  ?\{ return return ?\} tab up tab])


;; --- C macros --- ;
(fset 'cmain
   [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?\) ?  ?\{ return return return return return ?\} up tab ?r ?e ?t ?u ?r ?n ?  ?0 ?\; up up tab])

(fset 'cmain2
   [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?b ?o ?o ?l ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?l ?i ?b ?. ?h ?> return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?\) ?  ?\{ return return return ?\} up tab ?r ?e ?t ?u ?r ?n ?  ?E ?X ?I ?T ?_ ?S ?U ?C ?C ?E ?S ?S ?\; up return return up tab])

(fset 'cmain3
   [?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?i ?o ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?b ?o ?o ?l ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?d ?l ?i ?b ?. ?h ?> return ?# ?i ?n ?c ?l ?u ?d ?e ?  ?< ?s ?t ?r ?i ?n ?g ?. ?h ?> return return return ?/ ?* ?* return ?  ?* return ?  ?* return ?  ?* return ?  ?* ?/ return return ?i ?n ?t ?  ?m ?a ?i ?n ?\( ?i ?n ?t ?  ?a ?r ?g ?c ?, ?  ?c ?h ?a ?r ?  ?* ?a ?r ?g ?v ?\[ ?\] ?\) ?  ?\{ return return return return return return ?\} up ?  ?  ?r ?e ?t ?u ?r ?n ?  ?E ?X ?I ?T ?_ ?S ?U ?C ?C ?E ?S ?S ?\; up up up ?  ? ])


(fset 'cfor
   [?f ?o ?r ?  ?\( ?i ?  ?= ?  ?0 ?\; ?  ?i ?  ?< ?  ?S ?I ?Z ?E ?\; ?  ?i ?+ ?+ ?\) ?  ?\{ return return ?\} up tab])

(fset 'cdd
   [tab ?/ ?* ?\S-  ?D ?E ?B ?U ?G ?  ?* ?/ return return tab ?/ ?* ?\S-  ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G ?  ?* ?/ up tab ?p ?r ?i ?n ?t ?f ?\( ?\" ?\\ ?n ?\" ?\) ?\; left left left left left])

(fset 'ccom
   [tab ?/ ?* ?  ?  ?* ?/ left left left])

;; --- CoffeeScript macros --- ;;
(fset 'cfdd
   [?# ?  ?D ?E ?B ?U ?G return ?E ?N ?D ?\S-  ?D ?E ?B ?U ?G up return backspace backspace ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ? ])

(fset 'cfcom
   [tab ?# return return return ?@ ?p ?a ?r ?a ?m ?  ?\[ ?T ?y ?p ?e ?\] ?  ?t return ?@ ?r ?e ?t ?u ?r ?n ?  ?\[ ?T ?y ?p ?e ?\] ?  ?t up up up down ?  up ?  up ? ])


(fset 'org-html-spruce
   [up up ?\C-k right right right right right right backspace backspace backspace backspace ?d ?i ?v escape ?< ?\C-k ?\C-k ?\C-k ?\C-k ?\C-k ?\C-k ?\C-k ?\C-e left left left left left return backspace backspace backspace backspace down ?\M-d ?d ?i ?v ?. ?c ?l ?o ?j ?u ?r ?e ?\C-a down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down left left left left left left left left left return backspace backspace backspace backspace down ?\C-k right right right right backspace backspace backspace backspace ?\C-e left left left left return backspace backspace backspace backspace down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down down right right right right return backspace backspace backspace backspace down ?\C-k right right backspace backspace right ?\C-k ?d ?i ?v ?  ?c ?l ?a ?s ?s ?= ?\" ?c ?l ?o ?j ?u ?r ?e ?\C-e ?> down down ?\C-a right right right right right return up up])

;; --- macro aliases --- ;;
(defalias 'clic 'lic)
(defalias 'jcom 'jdoc)
(defalias 'jsl  'jslambda)
