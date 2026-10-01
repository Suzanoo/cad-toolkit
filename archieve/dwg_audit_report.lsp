(vl-load-com)

;; =========================================================
;; DWG AUDIT REPORT (READ-ONLY)
;; Purpose:
;;   - Inspect current DWG
;;   - Generate text report
;;   - DO NOT modify drawing
;;
;; Command:
;;   AUDITREPORT
;;
;; Output:
;;   <drawing folder>\<drawing name>_audit_report.txt
;; =========================================================

(defun _writeln (f s)
  (write-line s f)
)

(defun _safe-str (x)
  (if x (vl-princ-to-string x) "")
)

(defun _yesno (x)
  (if x "Yes" "No")
)

(defun _repeat-char (ch n / out i)
  (setq out "")
  (setq i 0)
  (repeat n
    (setq out (strcat out ch))
    (setq i (1+ i))
  )
  out
)

(defun _heading (f title)
  (_writeln f "")
  (_writeln f (_repeat-char "=" 70))
  (_writeln f title)
  (_writeln f (_repeat-char "=" 70))
)

(defun _subheading (f title)
  (_writeln f "")
  (_writeln f (_repeat-char "-" 50))
  (_writeln f title)
  (_writeln f (_repeat-char "-" 50))
)

(defun _getvar-safe (name)
  (vl-catch-all-apply 'getvar (list name))
)

(defun _var-ok (v)
  (not (vl-catch-all-error-p v))
)

(defun _insunits-name (n)
  (cond
    ((= n 0) "Unitless")
    ((= n 1) "Inches")
    ((= n 2) "Feet")
    ((= n 3) "Miles")
    ((= n 4) "Millimeters")
    ((= n 5) "Centimeters")
    ((= n 6) "Meters")
    ((= n 7) "Kilometers")
    ((= n 8) "Microinches")
    ((= n 9) "Mils")
    ((= n 10) "Yards")
    ((= n 11) "Angstroms")
    ((= n 12) "Nanometers")
    ((= n 13) "Microns")
    ((= n 14) "Decimeters")
    ((= n 15) "Decameters")
    ((= n 16) "Hectometers")
    ((= n 17) "Gigameters")
    ((= n 18) "Astronomical units")
    ((= n 19) "Light years")
    ((= n 20) "Parsecs")
    (T "Unknown")
  )
)

(defun _lunits-name (n)
  (cond
    ((= n 1) "Scientific")
    ((= n 2) "Decimal")
    ((= n 3) "Engineering")
    ((= n 4) "Architectural")
    ((= n 5) "Fractional")
    (T "Unknown")
  )
)

(defun _fmt-pt (p)
  (if p
    (strcat "("
            (rtos (car p) 2 6) ", "
            (rtos (cadr p) 2 6) ", "
            (rtos (if (caddr p) (caddr p) 0.0) 2 6) ")")
    "N/A"
  )
)

(defun _distance2d (p1 p2)
  (if (and p1 p2)
    (distance (list (car p1) (cadr p1) 0.0)
              (list (car p2) (cadr p2) 0.0))
    0.0
  )
)

(defun _count-ss (flt / ss)
  (setq ss (ssget "_X" flt))
  (if ss (sslength ss) 0)
)

(defun _count-all-entities (/ ss)
  (setq ss (ssget "_X"))
  (if ss (sslength ss) 0)
)

(defun _layer-list (/ layers rec out)
  (setq layers '())
  (setq rec (tblnext "LAYER" T))
  (while rec
    (setq layers (cons (cdr (assoc 2 rec)) layers))
    (setq rec (tblnext "LAYER"))
  )
  (reverse layers)
)

(defun _block-list-nonxref (/ blocks rec name flag out)
  (setq out '())
  (setq rec (tblnext "BLOCK" T))
  (while rec
    (setq name (cdr (assoc 2 rec)))
    (setq flag (cdr (assoc 70 rec)))
    ;; skip model/paper space, xrefs, anonymous/layout-related when possible
    (if (and name
             (/= (strcase name) "*MODEL_SPACE")
             (/= (strcase name) "*PAPER_SPACE")
             (/= (substr name 1 1) "*")
             (= (logand flag 4) 0)) ; not xref
      (setq out (cons name out))
    )
    (setq rec (tblnext "BLOCK"))
  )
  (reverse out)
)

(defun _xref-list (/ blocks rec name flag path out)
  (setq out '())
  (setq rec (tblnext "BLOCK" T))
  (while rec
    (setq name (cdr (assoc 2 rec)))
    (setq flag (cdr (assoc 70 rec)))
    (setq path (cdr (assoc 1 rec)))
    (if (= (logand flag 4) 4)
      (setq out (cons (list name path) out))
    )
    (setq rec (tblnext "BLOCK"))
  )
  (reverse out)
)

(defun _count-nonzero-z (/ ss i e ed typ p1 p2 p inspt elev cnt)
  (setq cnt 0)
  (setq ss (ssget "_X"))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (setq ed (entget e))
        (setq typ (cdr (assoc 0 ed)))

        ;; Check common elevation / coordinate patterns
        (cond
          ;; entities with elevation code 38
          ((assoc 38 ed)
            (setq elev (cdr (assoc 38 ed)))
            (if (and elev (/= (abs elev) 0.0))
              (setq cnt (1+ cnt))
            )
          )

          ;; LINE: start 10 / end 11
          ((= typ "LINE")
            (setq p1 (cdr (assoc 10 ed)))
            (setq p2 (cdr (assoc 11 ed)))
            (if (or (/= (abs (if (caddr p1) (caddr p1) 0.0)) 0.0)
                    (/= (abs (if (caddr p2) (caddr p2) 0.0)) 0.0))
              (setq cnt (1+ cnt))
            )
          )

          ;; POINT / TEXT / MTEXT / INSERT / CIRCLE / ARC etc.
          ((assoc 10 ed)
            (setq p (cdr (assoc 10 ed)))
            (if (/= (abs (if (caddr p) (caddr p) 0.0)) 0.0)
              (setq cnt (1+ cnt))
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )
  cnt
)

(defun _layer-has-name (nm / rec found)
  (setq found nil)
  (setq rec (tblnext "LAYER" T))
  (while rec
    (if (= (strcase (cdr (assoc 2 rec))) (strcase nm))
      (setq found T)
    )
    (setq rec (tblnext "LAYER"))
  )
  found
)

(defun _largest-abs-coordinate (/ extmin extmax vals m)
  (setq extmin (getvar "EXTMIN"))
  (setq extmax (getvar "EXTMAX"))
  (setq vals
    (list
      (abs (car extmin))
      (abs (cadr extmin))
      (abs (if (caddr extmin) (caddr extmin) 0.0))
      (abs (car extmax))
      (abs (cadr extmax))
      (abs (if (caddr extmax) (caddr extmax) 0.0))
    )
  )
  (apply 'max vals)
)

(defun _recommendation-list
  (insunits layercount xrefcount nonzeroz maxabscoord textcount dimcount hatchcount / recs)
  (setq recs '())

  (if (= insunits 0)
    (setq recs (cons "- INSUNITS = Unitless -> ควรกำหนดหน่วยให้ชัดก่อนเข้า Revit" recs))
  )

  (if (> maxabscoord 100000.0)
    (setq recs (cons "- พิกัดอยู่ไกล origin มาก -> เสี่ยงต่อปัญหาใน Revit" recs))
  )

  (if (> layercount 100)
    (setq recs (cons "- จำนวน Layer ค่อนข้างมาก -> ควร review และ purge layer ที่ไม่ใช้" recs))
  )

  (if (> xrefcount 0)
    (setq recs (cons "- พบ XREF -> ต้องตัดสินใจก่อนว่าจะ bind / detach / keep reference" recs))
  )

  (if (> nonzeroz 0)
    (setq recs (cons "- พบ object ที่ Z ไม่เป็น 0 -> ควร flatten/check ก่อนเข้า Revit" recs))
  )

  (if (> textcount 0)
    (setq recs (cons "- พบ TEXT/MTEXT -> ถ้าใช้แค่เป็น reference อาจควรแยก/ซ่อน" recs))
  )

  (if (> dimcount 0)
    (setq recs (cons "- พบ DIMENSION -> ปกติไม่ควรเอาเข้า Revit โดยไม่จำเป็น" recs))
  )

  (if (> hatchcount 0)
    (setq recs (cons "- พบ HATCH -> อาจทำให้ไฟล์หนัก ควรพิจารณาลบ/แยก" recs))
  )

  (if (_layer-has-name "DEFPOINTS")
    (setq recs (cons "- พบ layer DEFPOINTS -> ตรวจว่ามี geometry ตกค้างหรือไม่" recs))
  )

  (if (_layer-has-name "0")
    (setq recs (cons "- มี layer 0 -> ตรวจว่า object สำคัญถูกวางรวมมั่วหรือไม่" recs))
  )

  (if (null recs)
    (setq recs (list "- ไม่พบประเด็นเด่นจากการตรวจเบื้องต้น"))
  )

  (reverse recs)
)

(defun c:AUDITREPORT
  (/ doc fullname dwgname folder outpath f
     acadver insunits lunits ltscale extmin extmax
     totalents layerlist layercount blocklist blockcount
     xreflist xrefcount linecount plinecount lwplinecount
     circlecount arccount textcount mtextcount dimcount
     hatchcount insertcount pointcount splinecount
     ellipsecount nonzeroz maxabscoord width height
     recs)

  (vl-load-com)

  (setq fullname (getvar "DWGNAME"))
  (setq folder   (getvar "DWGPREFIX"))

  (if (or (null fullname) (= fullname ""))
    (progn
      (princ "\nPlease save the drawing first.")
      (princ)
    )
    (progn
      (setq dwgname (vl-filename-base fullname))
      (setq outpath (strcat folder dwgname "_audit_report.txt"))

      (setq f (open outpath "w"))

      ;; basic vars
      (setq acadver  (getvar "ACADVER"))
      (setq insunits (getvar "INSUNITS"))
      (setq lunits   (getvar "LUNITS"))
      (setq ltscale  (getvar "LTSCALE"))
      (setq extmin   (getvar "EXTMIN"))
      (setq extmax   (getvar "EXTMAX"))

      ;; counts
      (setq totalents     (_count-all-entities))
      (setq layerlist     (_layer-list))
      (setq layercount    (length layerlist))
      (setq blocklist     (_block-list-nonxref))
      (setq blockcount    (length blocklist))
      (setq xreflist      (_xref-list))
      (setq xrefcount     (length xreflist))

      (setq linecount     (_count-ss '((0 . "LINE"))))
      (setq plinecount    (_count-ss '((0 . "POLYLINE"))))
      (setq lwplinecount  (_count-ss '((0 . "LWPOLYLINE"))))
      (setq circlecount   (_count-ss '((0 . "CIRCLE"))))
      (setq arccount      (_count-ss '((0 . "ARC"))))
      (setq textcount     (_count-ss '((0 . "TEXT"))))
      (setq mtextcount    (_count-ss '((0 . "MTEXT"))))
      (setq dimcount      (_count-ss '((0 . "DIMENSION"))))
      (setq hatchcount    (_count-ss '((0 . "HATCH"))))
      (setq insertcount   (_count-ss '((0 . "INSERT"))))
      (setq pointcount    (_count-ss '((0 . "POINT"))))
      (setq splinecount   (_count-ss '((0 . "SPLINE"))))
      (setq ellipsecount  (_count-ss '((0 . "ELLIPSE"))))

      (setq nonzeroz      (_count-nonzero-z))
      (setq maxabscoord   (_largest-abs-coordinate))
      (setq width         (abs (- (car extmax) (car extmin))))
      (setq height        (abs (- (cadr extmax) (cadr extmin))))

      (setq recs (_recommendation-list
                    insunits layercount xrefcount nonzeroz
                    maxabscoord (+ textcount mtextcount)
                    dimcount hatchcount))

      ;; write file
      (_writeln f "DWG AUDIT REPORT")
      (_writeln f (_repeat-char "=" 70))
      (_writeln f (strcat "File Name      : " fullname))
      (_writeln f (strcat "Folder         : " folder))
      (_writeln f (strcat "Report File    : " outpath))
      (_writeln f (strcat "AutoCAD Ver.   : " (_safe-str acadver)))
      (_writeln f (strcat "Generated Time : " (menucmd "M=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)")))

      (_heading f "1) DRAWING SETTINGS")
      (_writeln f (strcat "INSUNITS       : " (itoa insunits) " (" (_insunits-name insunits) ")"))
      (_writeln f (strcat "LUNITS         : " (itoa lunits) " (" (_lunits-name lunits) ")"))
      (_writeln f (strcat "LTSCALE        : " (_safe-str ltscale)))

      (_heading f "2) DRAWING EXTENTS / COORDINATES")
      (_writeln f (strcat "EXTMIN         : " (_fmt-pt extmin)))
      (_writeln f (strcat "EXTMAX         : " (_fmt-pt extmax)))
      (_writeln f (strcat "Approx Width   : " (rtos width 2 6)))
      (_writeln f (strcat "Approx Height  : " (rtos height 2 6)))
      (_writeln f (strcat "Max |coord|    : " (rtos maxabscoord 2 6)))
      (_writeln f (strcat "Far from origin? : " (_yesno (> maxabscoord 100000.0))))

      (_heading f "3) ENTITY SUMMARY")
      (_writeln f (strcat "Total entities : " (itoa totalents)))
      (_writeln f (strcat "LINE           : " (itoa linecount)))
      (_writeln f (strcat "POLYLINE       : " (itoa plinecount)))
      (_writeln f (strcat "LWPOLYLINE     : " (itoa lwplinecount)))
      (_writeln f (strcat "CIRCLE         : " (itoa circlecount)))
      (_writeln f (strcat "ARC            : " (itoa arccount)))
      (_writeln f (strcat "SPLINE         : " (itoa splinecount)))
      (_writeln f (strcat "ELLIPSE        : " (itoa ellipsecount)))
      (_writeln f (strcat "POINT          : " (itoa pointcount)))
      (_writeln f (strcat "INSERT(Block)  : " (itoa insertcount)))
      (_writeln f (strcat "TEXT           : " (itoa textcount)))
      (_writeln f (strcat "MTEXT          : " (itoa mtextcount)))
      (_writeln f (strcat "DIMENSION      : " (itoa dimcount)))
      (_writeln f (strcat "HATCH          : " (itoa hatchcount)))

      (_heading f "4) LAYERS")
      (_writeln f (strcat "Total layers   : " (itoa layercount)))
      (_writeln f (strcat "Has layer 0    : " (_yesno (_layer-has-name "0"))))
      (_writeln f (strcat "Has DEFPOINTS  : " (_yesno (_layer-has-name "DEFPOINTS"))))
      (_subheading f "Layer Names")
      (foreach lay layerlist
        (_writeln f (strcat " - " lay))
      )

      (_heading f "5) BLOCKS")
      (_writeln f (strcat "Named blocks (non-xref): " (itoa blockcount)))
      (_subheading f "Block Names")
      (if blocklist
        (foreach b blocklist
          (_writeln f (strcat " - " b))
        )
        (_writeln f " - None")
      )

      (_heading f "6) XREFS")
      (_writeln f (strcat "Xref count     : " (itoa xrefcount)))
      (_subheading f "Xref Details")
      (if xreflist
        (foreach x xreflist
          (_writeln f
            (strcat " - Name: " (_safe-str (car x))
                    " | Path: " (_safe-str (cadr x))))
        )
        (_writeln f " - None")
      )

      (_heading f "7) GEOMETRY RISK CHECK")
      (_writeln f (strcat "Objects with non-zero Z (approx check) : " (itoa nonzeroz)))
      (_writeln f (strcat "Potential 3D / elevated content?       : " (_yesno (> nonzeroz 0))))

      (_heading f "8) BASIC RECOMMENDATIONS")
      (foreach r recs
        (_writeln f r)
      )

      (_heading f "9) NOTES")
      (_writeln f "- This report is READ-ONLY. It does not clean or modify the DWG.")
      (_writeln f "- Non-zero Z check is a basic screening, not a full forensic validation.")
      (_writeln f "- Next step can be a separate CLEAN script after reviewing this report.")

      (close f)

      (princ (strcat "\nAudit report created: " outpath))
      (princ)
    )
  )
)