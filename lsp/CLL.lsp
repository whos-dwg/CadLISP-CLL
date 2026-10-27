;;-------------------=={ A1-CL-LEVEL CLL FCL/FFL Auto Place v23 }==-------------------;;
;;---------------------------------------------------------------------------------------;;

(vl-load-com)

(setq *A1CL_BlockName*        "A1-CL-LEVEL")
(setq *A1CL_AttrName*         "LEVEL")
(setq *A1CL_FlipProps*        (list "FLIP STATE2" "FLIP STATE3"))
(setq *A1CL_FlipRadius*       300.0)
(setq *A1CL_ExistTol*         1.0)
(setq *A1CL_FlipCloseValue*   1)
(setq *A1CL_FlipClearValue*   0)
(setq *A1CL_AngTol*           (* pi (/ 8.0 180.0)))
(setq *A1CL_MinLineLen*       1.0)
(if (not (boundp '*A1CL_ZeroBase*)) (setq *A1CL_ZeroBase* nil))
(if (or (not (boundp '*A1CL_LastMode*))
        (not (member *A1CL_LastMode* '("FCL" "FFL"))))
  (setq *A1CL_LastMode* "FCL"))
(setq *A1CL_PresetCorner* nil)

(defun A1CL:pt3 (p)
  (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
)

(defun A1CL:v+ (a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)) (+ (caddr (A1CL:pt3 a)) (caddr (A1CL:pt3 b))))
)

(defun A1CL:v- (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr (A1CL:pt3 a)) (caddr (A1CL:pt3 b))))
)

(defun A1CL:v* (v s)
  (list (* (car v) s) (* (cadr v) s) (* (caddr (A1CL:pt3 v)) s))
)

(defun A1CL:dot2 (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

(defun A1CL:vlen2 (v)
  (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v))))
)

(defun A1CL:unit2 (v / d)
  (setq d (A1CL:vlen2 v))
  (if (> d 1e-9)
    (list (/ (car v) d) (/ (cadr v) d) 0.0)
    nil
  )
)

(defun A1CL:ang-pi (a)
  (while (< a 0.0) (setq a (+ a pi)))
  (while (>= a pi) (setq a (- a pi)))
  a
)

(defun A1CL:angdiff-pi (a b / d)
  (setq a (A1CL:ang-pi a))
  (setq b (A1CL:ang-pi b))
  (setq d (abs (- a b)))
  (if (> d (/ pi 2.0)) (setq d (- pi d)))
  d
)

(defun A1CL:norm-name (s / out i ch code)
  (setq s (strcase s))
  (setq out "")
  (setq i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (setq code (ascii ch))
    (if (or (and (>= code 48) (<= code 57)) (and (>= code 65) (<= code 90)))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  out
)

(defun A1CL:get-space (/ doc)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)
  )
)

(defun A1CL:eff-name (blk / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list blk)))
  (if (vl-catch-all-error-p r) (vla-get-Name blk) r)
)

(defun A1CL:same-block-p (obj)
  (and
    obj
    (= (vla-get-ObjectName obj) "AcDbBlockReference")
    (= (strcase (A1CL:eff-name obj)) (strcase *A1CL_BlockName*))
  )
)

(defun A1CL:get-ins-ucs (blk / r p)
  (setq r (vl-catch-all-apply 'vla-get-InsertionPoint (list blk)))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (setq p (vlax-safearray->list (vlax-variant-value r)))
      (trans p 0 1)
    )
  )
)

(defun A1CL:same-object-p (a b / ida idb)
  (if (and a b)
    (progn
      (setq ida (vl-catch-all-apply 'vla-get-ObjectID (list a)))
      (setq idb (vl-catch-all-apply 'vla-get-ObjectID (list b)))
      (and (not (vl-catch-all-error-p ida)) (not (vl-catch-all-error-p idb)) (= ida idb))
    )
    nil
  )
)

(defun A1CL:near-same-block-p (pt rad self / p1 p2 ss i obj ip found)
  (setq p1 (list (- (car pt) rad) (- (cadr pt) rad) 0.0))
  (setq p2 (list (+ (car pt) rad) (+ (cadr pt) rad) 0.0))
  (setq ss (ssget "_C" p1 p2 '((0 . "INSERT"))))
  (setq found nil)
  (if ss
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (not found))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (if (and (A1CL:same-block-p obj) (not (A1CL:same-object-p obj self)))
          (progn
            (setq ip (A1CL:get-ins-ucs obj))
            (if (and ip (<= (distance pt ip) rad))
              (setq found T)
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  found
)

(defun A1CL:set-attr (blk tag val / atts a done target actual)
  (setq done nil)
  (setq target (A1CL:norm-name tag))
  (if (= :vlax-true (vla-get-HasAttributes blk))
    (progn
      (setq atts (vlax-invoke blk 'GetAttributes))
      (foreach a atts
        (setq actual (A1CL:norm-name (vla-get-TagString a)))
        (if (or (= actual target)
                (and (= target "TYP") (= actual "TYPE"))
                (and (= target "LEVEL") (= actual "LEVLE"))
                (and (= target "LEVLE") (= actual "LEVEL")))
          (progn
            (vla-put-TextString a val)
            (setq done T)
          )
        )
      )
    )
  )
  done
)

(defun A1CL:set-dyn (blk prop val / props p pn tn ok)
  (setq ok nil)
  (setq tn (A1CL:norm-name prop))
  (setq props (vl-catch-all-apply 'vlax-invoke (list blk 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p props))
    (foreach p props
      (setq pn (A1CL:norm-name (vla-get-PropertyName p)))
      (if (or (= pn tn)
              (and (= tn "FLIPSTATE1") (= pn "LIPSTATE1"))
              (and (= tn "FLIPSTATE2") (= pn "LIPSTATE2"))
              (and (= tn "FLIPSTATE3") (= pn "LIPSTATE3")))
        (progn
          (vl-catch-all-apply
            'vla-put-Value
            (list p (vlax-make-variant val (vlax-variant-type (vla-get-Value p))))
          )
          (setq ok T)
        )
      )
    )
  )
  ok
)

(defun A1CL:apply-flip (blk doflip / val prop)
  (setq val (if doflip *A1CL_FlipCloseValue* *A1CL_FlipClearValue*))
  (foreach prop *A1CL_FlipProps*
    (A1CL:set-dyn blk prop val)
  )
)

(defun A1CL:near-pt-list-p (pt pts rad / found q)
  (setq found nil)
  (foreach q pts
    (if (and (not found) (<= (distance pt q) rad))
      (setq found T)
    )
  )
  found
)

(defun A1CL:pt-member-p (pt pts tol / found q)
  (setq found nil)
  (foreach q pts
    (if (and (not found) (<= (distance pt q) tol))
      (setq found T)
    )
  )
  found
)

(defun A1CL:component-pts (pt pts rad / comp queue q r d)
  (setq comp nil)
  (setq queue (list pt))
  (while queue
    (setq q (car queue))
    (setq queue (cdr queue))
    (if (not (A1CL:pt-member-p q comp *A1CL_ExistTol*))
      (progn
        (setq comp (cons q comp))
        (foreach r pts
          (setq d (distance q r))
          (if (and
                (> d *A1CL_ExistTol*)
                (<= d rad)
                (not (A1CL:pt-member-p r comp *A1CL_ExistTol*))
                (not (A1CL:pt-member-p r queue *A1CL_ExistTol*))
              )
            (setq queue (cons r queue))
          )
        )
      )
    )
  )
  comp
)

(defun A1CL:top-priority-p (pt comp / ok q dy)
  (setq ok T)
  (foreach q comp
    (if (and (> (distance pt q) *A1CL_ExistTol*) ok)
      (progn
        (setq dy (- (cadr q) (cadr pt)))
        (if (or
              (> dy *A1CL_ExistTol*)
              (and (<= (abs dy) *A1CL_ExistTol*) (< (car q) (- (car pt) *A1CL_ExistTol*)))
            )
          (setq ok nil)
        )
      )
    )
  )
  ok
)

(defun A1CL:flip-upper-of-each-pair-p (pt pts / found q dy)
  (setq found nil)
  (foreach q pts
    (if (and
          (not found)
          (> (distance pt q) *A1CL_ExistTol*)
          (<= (distance pt q) *A1CL_FlipRadius*)
          (< (cadr q) (- (cadr pt) *A1CL_ExistTol*))
        )
      (setq found T)
    )
  )
  found
)

(defun A1CL:round-to-5 (value / scaled)
  ;; Round symmetrically to the nearest multiple of 5.  AutoLISP FIX
  ;; truncates toward zero, so negative values need the opposite offset.
  (setq scaled (/ value 5.0))
  (* 5.0
    (if (>= scaled 0.0)
      (fix (+ scaled 0.5))
      (fix (- scaled 0.5))
    )
  )
)

(defun A1CL:insert-one (pt base doflip / spc blk level levelval)
  (setq levelval (A1CL:round-to-5 (- (cadr pt) (cadr base))))
  (setq level (rtos levelval 2 0))
  (setq spc (A1CL:get-space))
  (setq blk
    (vl-catch-all-apply
      'vla-InsertBlock
      (list spc (vlax-3d-point (A1CL:pt3 (trans pt 1 0))) *A1CL_BlockName* 1.0 1.0 1.0 0.0)
    )
  )
  (if (vl-catch-all-error-p blk)
    nil
    (progn
      (A1CL:set-attr blk *A1CL_AttrName* level)
      (A1CL:apply-flip blk doflip)
      (vla-Update blk)
      blk
    )
  )
)

(defun A1CL:dxf (ed code)
  (cdr (assoc code ed))
)

(defun A1CL:dim-type-base (ed / v)
  (setq v (A1CL:dxf ed 70))
  (if v (logand v 7) nil)
)

(defun A1CL:dim-line-angle (ed p13 p14 / typ a)
  (setq typ (A1CL:dim-type-base ed))
  (setq a (A1CL:dxf ed 50))
  (cond
    ((and (= typ 0) a) a)
    ((and p13 p14) (angle p13 p14))
    (a a)
    (T nil)
  )
)

(defun A1CL:ext-end (src dimlinept dimang exe / n offset q dir)
  (setq n (list (- (sin dimang)) (cos dimang) 0.0))
  (setq offset (A1CL:dot2 (A1CL:v- dimlinept src) n))
  (setq q (A1CL:v+ src (A1CL:v* n offset)))
  (setq dir (A1CL:unit2 (A1CL:v- q src)))
  (if dir
    (A1CL:v+ q (A1CL:v* dir exe))
    q
  )
)

(defun A1CL:get-line-data (obj / sp ep len ang)
  (if (= (vla-get-ObjectName obj) "AcDbLine")
    (progn
      (setq sp (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
      (setq ep (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
      (setq len (distance sp ep))
      (if (> len *A1CL_MinLineLen*)
        (progn
          (setq ang (A1CL:ang-pi (angle sp ep)))
          (list sp ep len ang)
        )
        nil
      )
    )
    nil
  )
)

(defun A1CL:line-sp (ln) (nth 0 ln))
(defun A1CL:line-ep (ln) (nth 1 ln))
(defun A1CL:line-len (ln) (nth 2 ln))
(defun A1CL:line-ang (ln) (nth 3 ln))

(defun A1CL:unique-pts (pts tol / out p found q)
  (setq out nil)
  (foreach p pts
    (setq found nil)
    (foreach q out
      (if (<= (distance p q) tol) (setq found T))
    )
    (if (not found) (setq out (cons p out)))
  )
  (reverse out)
)

(defun A1CL:line-mid (ln)
  (A1CL:v* (A1CL:v+ (A1CL:line-sp ln) (A1CL:line-ep ln)) 0.5)
)

(defun A1CL:block-lines-from-dim (ent / ed bname btr e ed2 typ p10 p11 lines)
  (setq ed (entget ent))
  (setq bname (cdr (assoc 2 ed)))
  (setq lines nil)
  (if (and bname (setq btr (tblobjname "BLOCK" bname)))
    (progn
      (setq e (entnext btr))
      (while e
        (setq ed2 (entget e))
        (setq typ (cdr (assoc 0 ed2)))
        (cond
          ((= typ "LINE")
            (setq p10 (cdr (assoc 10 ed2)))
            (setq p11 (cdr (assoc 11 ed2)))
            (if (and p10 p11 (> (distance p10 p11) *A1CL_MinLineLen*))
              (setq lines (cons (list p10 p11 (distance p10 p11) (A1CL:ang-pi (angle p10 p11))) lines))
            )
          )
        )
        (setq e (entnext e))
      )
    )
  )
  lines
)

(defun A1CL:pick-extension-ends-from-lines (lines dimang dimlinept / dimg extang cand n ref u sorted grouped pts ln pA pB da db mid proj groups g existing)
  (setq lines (vl-remove-if 'null lines))
  (if (< (length lines) 1)
    nil
    (progn
      (if dimang
        (setq dimg (A1CL:ang-pi dimang))
        (setq dimg (A1CL:line-ang (A1CL:longest-line lines)))
      )
      (setq extang (A1CL:ang-pi (+ dimg (/ pi 2.0))))
      (setq cand nil)
      (foreach ln lines
        (if (<= (A1CL:angdiff-pi (A1CL:line-ang ln) extang) *A1CL_AngTol*)
          (setq cand (cons ln cand))
        )
      )
      (if (not cand)
        nil
        (progn
          (setq n (list (- (sin dimg)) (cos dimg) 0.0))
          (setq u (list (cos dimg) (sin dimg) 0.0))
          (if (not dimlinept)
            (progn
              (setq ref nil)
              (foreach ln lines
                (if (and (not ref) (<= (A1CL:angdiff-pi (A1CL:line-ang ln) dimg) *A1CL_AngTol*))
                  (setq ref (A1CL:line-mid ln))
                )
              )
              (if (not ref) (setq ref (A1CL:line-mid (A1CL:longest-line lines))))
            )
            (setq ref dimlinept)
          )
          (setq sorted
            (vl-sort cand
              (function
                (lambda (a b)
                  (< (A1CL:dot2 (A1CL:v- (A1CL:line-mid a) ref) u)
                     (A1CL:dot2 (A1CL:v- (A1CL:line-mid b) ref) u))
                )
              )
            )
          )
          (setq pts nil)
          (foreach ln sorted
            (setq pA (A1CL:line-sp ln))
            (setq pB (A1CL:line-ep ln))
            (setq da (abs (A1CL:dot2 (A1CL:v- pA ref) n)))
            (setq db (abs (A1CL:dot2 (A1CL:v- pB ref) n)))
            (setq pts (cons (if (< da db) pA pB) pts))
          )
          (setq pts (A1CL:unique-pts pts 1.0))
          (cond
            ((> (length pts) 2)
              (setq pts
                (vl-sort pts
                  (function
                    (lambda (a b)
                      (< (A1CL:dot2 (A1CL:v- a ref) u)
                         (A1CL:dot2 (A1CL:v- b ref) u))
                    )
                  )
                )
              )
              (list (car pts) (car (last pts)))
            )
            (T pts)
          )
        )
      )
    )
  )
)

(defun A1CL:longest-line (lines / sorted)
  (setq sorted (vl-sort lines '(lambda (a b) (> (A1CL:line-len a) (A1CL:line-len b)))))
  (car sorted)
)

(defun A1CL:infer-ext-ends-from-lines (lines dimang / dimg extang cand sorted dimline dimsp n pA pB da db result)
  (setq lines (vl-remove-if 'null lines))
  (if (< (length lines) 2)
    nil
    (progn
      (if dimang
        (setq dimg (A1CL:ang-pi dimang))
        (setq dimg (A1CL:line-ang (A1CL:longest-line lines)))
      )
      (setq extang (A1CL:ang-pi (+ dimg (/ pi 2.0))))
      (setq cand nil)
      (foreach ln lines
        (if (<= (A1CL:angdiff-pi (A1CL:line-ang ln) extang) *A1CL_AngTol*)
          (setq cand (cons ln cand))
        )
      )
      (setq sorted (vl-sort cand '(lambda (a b) (> (A1CL:line-len a) (A1CL:line-len b)))))
      (if (< (length sorted) 2)
        nil
        (progn
          (setq dimline nil)
          (foreach ln lines
            (if (and (not dimline) (<= (A1CL:angdiff-pi (A1CL:line-ang ln) dimg) *A1CL_AngTol*))
              (setq dimline ln)
            )
          )
          (if (not dimline) (setq dimline (A1CL:longest-line lines)))
          (setq dimsp (A1CL:line-sp dimline))
          (setq n (list (- (sin dimg)) (cos dimg) 0.0))
          (setq result nil)
          (foreach ln (list (car sorted) (cadr sorted))
            (setq pA (A1CL:line-sp ln))
            (setq pB (A1CL:line-ep ln))
            (setq da (abs (A1CL:dot2 (A1CL:v- pA dimsp) n)))
            (setq db (abs (A1CL:dot2 (A1CL:v- pB dimsp) n)))
            (setq result (cons (if (< da db) pA pB) result))
          )
          (reverse result)
        )
      )
    )
  )
)

(defun A1CL:explode-dim-ext-ends-ucs (ent dimang / obj tmp var arr objs lines ld ends)
  (setq obj (vlax-ename->vla-object ent))
  (setq tmp (vl-catch-all-apply 'vla-Copy (list obj)))
  (if (vl-catch-all-error-p tmp)
    nil
    (progn
      (setq var (vl-catch-all-apply 'vla-Explode (list tmp)))
      (if (vl-catch-all-error-p var)
        (progn
          (vl-catch-all-apply 'vla-Delete (list tmp))
          nil
        )
        (progn
          (setq arr (vlax-variant-value var))
          (setq objs (vlax-safearray->list arr))
          (setq lines nil)
          (foreach o objs
            (setq ld (vl-catch-all-apply 'A1CL:get-line-data (list o)))
            (if (and (not (vl-catch-all-error-p ld)) ld)
              (setq lines (cons ld lines))
            )
          )
          (setq ends (A1CL:infer-ext-ends-from-lines lines dimang))
          (foreach o objs (vl-catch-all-apply 'vla-Delete (list o)))
          (vl-catch-all-apply 'vla-Delete (list tmp))
          (if ends
            (mapcar '(lambda (p) (trans p 0 1)) ends)
            nil
          )
        )
      )
    )
  )
)

(defun A1CL:get-dim-ext-ends-ucs (ent / ed p13 p14 p10 dimang exe r1 r2 lines ends direct)
  (if (not (= (type ent) 'ENAME))
    nil
    (progn
      (setq ed (entget ent))
      (setq p13 (A1CL:dxf ed 13))
      (setq p14 (A1CL:dxf ed 14))
      (setq p10 (A1CL:dxf ed 10))
      (setq dimang (A1CL:dim-line-angle ed p13 p14))
      (setq direct nil)

      (if (and p13 p14 p10 dimang)
        (progn
          (setq exe (getvar "DIMEXE"))
          (if (not exe) (setq exe 0.0))
          (setq r1 (vl-catch-all-apply 'A1CL:ext-end (list p13 p10 dimang exe)))
          (setq r2 (vl-catch-all-apply 'A1CL:ext-end (list p14 p10 dimang exe)))
          (if (and (not (vl-catch-all-error-p r1)) (not (vl-catch-all-error-p r2)) r1 r2)
            (setq direct (list (trans r1 0 1) (trans r2 0 1)))
          )
        )
      )

      (if direct
        direct
        (progn
          (setq lines (A1CL:block-lines-from-dim ent))
          (setq ends (A1CL:pick-extension-ends-from-lines lines dimang p10))
          (if ends
            (mapcar '(lambda (p) (trans p 0 1)) ends)
            nil
          )
        )
      )
    )
  )
)

(defun A1CL:place-point (pt base doflip / r)
  (cond
    ((A1CL:near-same-block-p pt *A1CL_ExistTol* nil) 'skipped)
    ((A1CL:insert-one pt base doflip) 'inserted)
    (T 'failed)
  )
)


(defun A1CL:pt-in-window-p (p a b / minx maxx miny maxy)
  (setq minx (min (car a) (car b)))
  (setq maxx (max (car a) (car b)))
  (setq miny (min (cadr a) (cadr b)))
  (setq maxy (max (cadr a) (cadr b)))
  (and
    (>= (car p) minx)
    (<= (car p) maxx)
    (>= (cadr p) miny)
    (<= (cadr p) maxy)
  )
)


(defun A1CL:sort-placement-pts (pts)
  (vl-sort pts
    (function
      (lambda (a b)
        (if (equal (cadr a) (cadr b) 1e-6)
          (< (car a) (car b))
          (> (cadr a) (cadr b))
        )
      )
    )
  )
)

(defun A1CL:run-fcl (/ *error* oldcmdecho oldosmode base p1 p2 ss i ent pts p res inserted skipped failed bad outside candidates doflip)
  (defun *error* (msg)
    (if oldosmode (setvar "OSMODE" oldosmode))
    (if oldcmdecho (setvar "CMDECHO" oldcmdecho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq oldcmdecho (getvar "CMDECHO"))
  (setq oldosmode  (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 1) ; Endpoint only

  (cond
    ((not (tblsearch "BLOCK" *A1CL_BlockName*))
      (princ (strcat "\nBlock definition not found: " *A1CL_BlockName*))
    )
    (T
      (setq p1 (A1CL:get-first-corner-v14))
      (if p1
        (progn
          (setq base *A1CL_ZeroBase*)
          (setq p2 (getcorner p1 "\nSpecify opposite window corner: "))
          (if p2
            (progn
              (setq ss (ssget "_C" p1 p2 '((0 . "DIMENSION"))))
              (if ss
                (progn
                  (setq inserted 0 skipped 0 failed 0 bad 0 outside 0 candidates nil)
                  (setq i 0)
                  (while (< i (sslength ss))
                    (setq ent (ssname ss i))
                    (setq pts (vl-catch-all-apply 'A1CL:get-dim-ext-ends-ucs (list ent)))
                    (if (or (vl-catch-all-error-p pts) (not pts))
                      (setq bad (1+ bad))
                      (foreach p pts
                        (if (A1CL:pt-in-window-p p p1 p2)
                          (setq candidates (cons p candidates))
                          (setq outside (1+ outside))
                        )
                      )
                    )
                    (setq i (1+ i))
                  )

                  (setq candidates (A1CL:unique-pts candidates *A1CL_ExistTol*))
                  (setq candidates (A1CL:sort-placement-pts candidates))
                  (foreach p candidates
                    (setq doflip (A1CL:flip-upper-of-each-pair-p p candidates))
                    (setq res (A1CL:place-point p base doflip))
                    (cond
                      ((= res 'inserted)
                        (setq inserted (1+ inserted))
                      )
                      ((= res 'skipped)  (setq skipped  (1+ skipped)))
                      ((= res 'failed)   (setq failed   (1+ failed)))
                    )
                  )

                  (princ
                    (strcat
                      "\nDone. Inserted: " (itoa inserted)
                      ", skipped existing: " (itoa skipped)
                      ", skipped outside: " (itoa outside)
                      ", failed: " (itoa failed)
                      ", unreadable dimensions: " (itoa bad) "."
                    )
                  )
                )
                (princ "\nNo DIMENSION objects found in the window.")
              )
            )
          )
        )
      )
    )
  )

  (setvar "OSMODE" oldosmode)
  (setvar "CMDECHO" oldcmdecho)
  (princ)
)

;; FFL helpers.  Coordinates are compared in the current UCS, matching the
;; existing placement logic.
(defun A1CL:lowest-point (pts / best p)
  (setq best nil)
  (foreach p pts
    (if (or (not best)
            (< (cadr p) (- (cadr best) *A1CL_ExistTol*))
            (and (<= (abs (- (cadr p) (cadr best))) *A1CL_ExistTol*)
                 (< (car p) (car best))))
      (setq best p)
    )
  )
  best
)

(defun A1CL:find-block-at (pt rad / p1 p2 ss i obj ip found)
  (setq p1 (list (- (car pt) rad) (- (cadr pt) rad) 0.0))
  (setq p2 (list (+ (car pt) rad) (+ (cadr pt) rad) 0.0))
  (setq ss (ssget "_C" p1 p2 '((0 . "INSERT"))))
  (setq i 0 found nil)
  (while (and ss (< i (sslength ss)) (not found))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (if (A1CL:same-block-p obj)
      (progn
        (setq ip (A1CL:get-ins-ucs obj))
        (if (and ip (<= (distance pt ip) rad)) (setq found obj))
      )
    )
    (setq i (1+ i))
  )
  found
)

(defun A1CL:configure-ffl (blk)
  (A1CL:set-dyn blk "FLIP STATE 1" 0)
  (A1CL:set-dyn blk "FLIP STATE 2" 0)
  (A1CL:set-dyn blk "FLIP STATE 3" 1)
  (A1CL:set-attr blk "TYP" "F.F.L.")
  (A1CL:set-attr blk "LEVEL" "")
  (A1CL:set-attr blk "LEVLE" "")
  (vla-Update blk)
  blk
)

(defun A1CL:insert-ffl (pt / spc blk)
  (setq spc (A1CL:get-space))
  (setq blk
    (vl-catch-all-apply
      'vla-InsertBlock
      (list spc (vlax-3d-point (A1CL:pt3 (trans pt 1 0)))
            *A1CL_BlockName* 1.0 1.0 1.0 0.0)
    )
  )
  (if (vl-catch-all-error-p blk)
    nil
    (progn
      (A1CL:configure-ffl blk)
    )
  )
)

(defun A1CL:get-first-corner-v14 (/ p1)
  (setq p1 *A1CL_PresetCorner*)
  (setq *A1CL_PresetCorner* nil)
  p1
)

(defun A1CL:run-ffl (/ *error* oldcmdecho oldosmode base p1 p2 ss i ent
                       pts p candidates lowestpt existing doflip res inserted skipped failed)
  (defun *error* (msg)
    (if oldosmode (setvar "OSMODE" oldosmode))
    (if oldcmdecho (setvar "CMDECHO" oldcmdecho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setq oldcmdecho (getvar "CMDECHO"))
  (setq oldosmode  (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 1)
  (cond
    ((not (tblsearch "BLOCK" *A1CL_BlockName*))
      (princ (strcat "\nBlock definition not found: " *A1CL_BlockName*))
    )
    ((setq p1 (A1CL:get-first-corner-v14))
      (setq base *A1CL_ZeroBase*)
      (if (setq p2 (getcorner p1 "\nFFL - Specify opposite window corner: "))
        (progn
          (setq ss (ssget "_C" p1 p2 '((0 . "DIMENSION"))))
          (if ss
            (progn
              (setq i 0 candidates nil inserted 0 skipped 0 failed 0)
              (while (< i (sslength ss))
                (setq ent (ssname ss i))
                (setq pts (vl-catch-all-apply 'A1CL:get-dim-ext-ends-ucs (list ent)))
                (if (and (not (vl-catch-all-error-p pts)) pts)
                  (foreach p pts
                    (if (A1CL:pt-in-window-p p p1 p2)
                      (setq candidates (cons p candidates))
                    )
                  )
                )
                (setq i (1+ i))
              )
              (setq candidates (A1CL:unique-pts candidates *A1CL_ExistTol*))
              (setq candidates (A1CL:sort-placement-pts candidates))
              (setq lowestpt (A1CL:lowest-point candidates))
              (if lowestpt
                (progn
                  ;; Preserve the original FCL placement at every point.  The
                  ;; only exception is the single lowest point, which is FFL.
                  (foreach p candidates
                    (cond
                      ((equal p lowestpt *A1CL_ExistTol*)
                        (setq existing (A1CL:find-block-at p *A1CL_ExistTol*))
                        (cond
                          (existing
                            ;; An FCL left by an earlier run must be converted,
                            ;; not skipped as a duplicate.
                            (A1CL:configure-ffl existing)
                            (setq inserted (1+ inserted)))
                          ((A1CL:insert-ffl p)
                            (setq inserted (1+ inserted)))
                          (T (setq failed (1+ failed)))
                        )
                      )
                      ((A1CL:near-same-block-p p *A1CL_ExistTol* nil)
                        (setq skipped (1+ skipped)))
                      (T
                        (setq doflip (A1CL:flip-upper-of-each-pair-p p candidates))
                        (setq res (A1CL:place-point p base doflip))
                        (cond
                          ((= res 'inserted) (setq inserted (1+ inserted)))
                          ((= res 'skipped)  (setq skipped  (1+ skipped)))
                          (T                 (setq failed   (1+ failed))))
                      )
                    )
                  )
                  (princ
                    (strcat "\nFFL done. Inserted/updated: " (itoa inserted)
                            ", skipped: " (itoa skipped)
                            ", failed: " (itoa failed) "."))
                )
                (princ "\nNo usable dimension extension endpoint found." )
              )
            )
            (princ "\nNo DIMENSION objects found in the window.")
          )
        )
      )
    )
  )
  (setvar "OSMODE" oldosmode)
  (setvar "CMDECHO" oldcmdecho)
  (princ)
)

(defun A1CL:settings (/ item newbase)
  (initget "FCL FFL Zero")
  (setq item
    (getkword
      (strcat "\nSettings [FCL/FFL/Zero] <"
              (if (= *A1CL_LastMode* "FFL") "FFL" "FCL") ">: ")))
  (if (not item) (setq item *A1CL_LastMode*))
  (cond
    ((or (= item "FCL") (= item "FFL"))
      (setq *A1CL_LastMode* item)
      (princ
        (strcat "\nMode set to "
                (if (= item "FFL") "FFL" "FCL") "."))
      ;; A zero point is mandatory on the first run, even when the user
      ;; chooses a mode before choosing Zero from the settings menu.
      (if (not *A1CL_ZeroBase*)
        (progn
          (setq newbase (getpoint "\nSpecify elevation zero point: "))
          (if newbase
            (progn
              (setq *A1CL_ZeroBase* newbase)
              (princ "\nElevation zero point saved."))
          )
        )
      )
    )
    ((= item "Zero")
      (setq newbase (getpoint "\nSpecify elevation zero point: "))
      (if newbase
        (progn
          (setq *A1CL_ZeroBase* newbase)
          (princ "\nElevation zero point saved."))
      )
    )
  )
  item
)

(defun c:CLL (/ ans corner)
  ;; CLL normally reuses both saved settings.  S is the only entry into setup.
  (setq corner nil)
  ;; First run must establish a mode and a zero point before selection.
  (if (not *A1CL_ZeroBase*) (A1CL:settings))
  (while (not corner)
    (initget "S")
    (setq ans (getpoint "\nSpecify first window corner or [Settings(S)]: "))
    (cond
      ((= ans "S") (A1CL:settings))
      ((and ans *A1CL_ZeroBase*) (setq corner ans))
      (ans (princ "\nZero point is not set. Enter S to configure it."))
      (T (setq corner 'cancelled))
    )
  )
  (if (and corner (not (= corner 'cancelled)))
    (progn
      (setq *A1CL_PresetCorner* corner)
      (if (= *A1CL_LastMode* "FFL")
        (A1CL:run-ffl)
        (A1CL:run-fcl)
      )
    )
  )
  (princ)
)

(princ "\nA1-CL-LEVEL v24 loaded. Command: CLL. Enter S for FCL, FFL, or Zero settings.")
(princ)
