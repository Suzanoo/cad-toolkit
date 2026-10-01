; FONT_DOCTOR_V3.lsp
; AutoCAD Font Doctor V3
; APPLOAD -> FONTFIX
;
; V3 changes:
; - Writes STYLE table records directly with entmod/entupd (not ActiveX FontFile).
; - Default dialog shows only suspect styles actually used by text-like objects.
; - "Show unused styles" checkbox can reveal zero-use styles.
; - One dialog: current font, object count, replacement alternatives, Keep Current, Browse.
; - Makes one UNDO group; never rewrites TEXT/MTEXT/ATTRIB content.
; - Verifies each style after writing and reports Fixed / Kept / Failed.

(vl-load-com)

(setq *fd3-common-fonts*
  '("Cordia New.ttf"
    "Angsana New.ttf"
    "Tahoma.ttf"
    "Arial.ttf"
    "Leelawadee UI.ttf"))

(defun fd3-empty-p (s)
  (or (null s) (= "" (vl-string-trim " " s))))

(defun fd3-lower (s)
  (if s (strcase s T) ""))

(defun fd3-ends-with (s suffix / a b)
  (setq s (fd3-lower s)
        suffix (fd3-lower suffix)
        a (strlen s)
        b (strlen suffix))
  (and (>= a b)
       (= (substr s (+ 1 (- a b))) suffix)))

(defun fd3-basename (p / x)
  (if (fd3-empty-p p)
    ""
    (progn
      (setq x (vl-string-translate "/" "\\" p))
      (if (vl-string-search "\\" x)
        (substr x (+ 2 (vl-string-position (ascii "\\") x 0 T)))
        x))))

(defun fd3-font-exists-p (f / base)
  (setq base (fd3-basename f))
  (and (not (fd3-empty-p f))
       (or (findfile f)
           (findfile base)
           ; Windows/AutoCAD can resolve registered TrueType fonts even when
           ; findfile does not expose the Windows Fonts directory.
           (member (fd3-lower base)
             '("arial.ttf" "tahoma.ttf" "leelawadee ui.ttf"
               "cordia new.ttf" "angsana new.ttf"
               "browallia new.ttf" "dilleniaupc.ttf"
               "eucrosiaupc.ttf" "freesiaupc.ttf"
               "irisupc.ttf" "jasmineupc.ttf"
               "kodchiangupc.ttf" "lilyupc.ttf")))))

(defun fd3-thai-name-p (s / x)
  (setq x (fd3-lower s))
  (or (wcmatch x "*cordia*")
      (wcmatch x "*angsana*")
      (wcmatch x "*browallia*")
      (wcmatch x "*dillenia*")
      (wcmatch x "*eucrosia*")
      (wcmatch x "*freesia*")
      (wcmatch x "*iris*")
      (wcmatch x "*jasmine*")
      (wcmatch x "*kodchiang*")
      (wcmatch x "*lily*")
      (wcmatch x "*thai*")))

(defun fd3-recommended (style font / x)
  (setq x (strcat (fd3-lower style) " " (fd3-lower font)))
  (cond
    ((wcmatch x "*cordia*") "Cordia New.ttf")
    ((wcmatch x "*angsana*") "Angsana New.ttf")
    ((wcmatch x "*browallia*") "Browallia New.ttf")
    ((wcmatch x "*dillenia*") "DilleniaUPC.ttf")
    ((wcmatch x "*eucrosia*") "EucrosiaUPC.ttf")
    ((wcmatch x "*freesia*") "FreesiaUPC.ttf")
    ((wcmatch x "*iris*") "IrisUPC.ttf")
    ((wcmatch x "*jasmine*") "JasmineUPC.ttf")
    ((wcmatch x "*kodchiang*") "KodchiangUPC.ttf")
    ((wcmatch x "*lily*") "LilyUPC.ttf")
    (T "Tahoma.ttf")))

(defun fd3-uniq (lst / out)
  (foreach x lst
    (if (and x (not (member x out)))
      (setq out (append out (list x)))))
  out)

(defun fd3-choices (style font / rec lst out)
  (setq rec (fd3-recommended style font)
        lst (fd3-uniq (append (list rec) *fd3-common-fonts*))
        out (list "<Keep Current>"))
  (foreach f lst
    (if (fd3-font-exists-p f)
      (setq out (append out (list f)))))
  (append out (list "<Browse...>")))

(defun fd3-default-choice (style font choices / rec)
  (setq rec (fd3-recommended style font))
  (cond
    ((member rec choices) rec)
    ((member "Tahoma.ttf" choices) "Tahoma.ttf")
    ((member "Arial.ttf" choices) "Arial.ttf")
    (T "<Keep Current>")))

(defun fd3-style-records (/ r out flags)
  (setq r (tblnext "STYLE" T))
  (while r
    (setq flags (cdr (assoc 70 r)))
    ; Skip shape-file STYLE records.
    (if (= 0 (logand (if flags flags 0) 1))
      (setq out
        (cons
          (list
            (cdr (assoc 2 r))
            (cdr (assoc 3 r))
            (cdr (assoc 4 r)))
          out)))
    (setq r (tblnext "STYLE")))
  (reverse out))

(defun fd3-suspect-p (style font big)
  (or
    (and (not (fd3-empty-p font))
         (not (fd3-font-exists-p font)))
    (fd3-thai-name-p style)
    (fd3-thai-name-p font)
    (and (fd3-ends-with font ".shx")
         (not (fd3-empty-p big))
         (not (fd3-font-exists-p big)))))

(defun fd3-count-style (style / ss)
  (setq ss
    (ssget "_X"
      (list
        '(-4 . "<OR")
          '(0 . "TEXT")
          '(0 . "MTEXT")
          '(0 . "ATTRIB")
          '(0 . "ATTDEF")
        '(-4 . "OR>")
        (cons 7 style))))
  (if ss (sslength ss) 0))

(defun fd3-question-count (/ ss i d txt n)
  (setq n 0
        ss (ssget "_X" '((0 . "TEXT,MTEXT,ATTRIB,ATTDEF"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq d (entget (ssname ss i))
              txt (cdr (assoc 1 d)))
        (if (and txt (wcmatch txt "*??*"))
          (setq n (1+ n)))
        (setq i (1+ i)))))
  n)

(defun fd3-replace-code (code value alist)
  (if (assoc code alist)
    (subst (cons code value) (assoc code alist) alist)
    (append alist (list (cons code value)))))

(defun fd3-write-style (style newfont / en ed newed verify)
  ; Directly modify DXF group 3 (primary font) and clear group 4 (Big Font).
  ; This is the V3 replacement for the ActiveX FontFile setter.
  (setq en (tblobjname "STYLE" style))
  (if (null en)
    nil
    (progn
      (setq ed (entget en)
            newed (fd3-replace-code 3 newfont ed)
            newed (fd3-replace-code 4 "" newed))
      (if (entmod newed)
        (progn
          (entupd en)
          (setq verify (entget en))
          (= (fd3-lower (cdr (assoc 3 verify)))
             (fd3-lower newfont)))
        nil))))

(defun fd3-write-dcl (path / f)
  (setq f (open path "w"))
  (foreach line
    '(
"fontdoctor3 : dialog {"
"  label = \"Font Doctor V3\";"
"  : column {"
"    : text { key=\"summary\"; width=86; }"
"    : boxed_column {"
"      label = \"Suspect Text Styles\";"
"      : text { label=\"Style | Current Font | Objects | Replacement\"; }"
"      : list_box { key=\"styles\"; width=86; height=13; }"
"      : toggle { key=\"unused\"; label=\"Show unused suspect styles (0 objects)\"; }"
"    }"
"    : row {"
"      : column {"
"        : text { label=\"Replacement for selected style:\"; }"
"        : popup_list { key=\"fonts\"; width=52; }"
"      }"
"    }"
"    : text { key=\"info\"; width=86; }"
"    : text { key=\"warning\"; width=86; }"
"    spacer;"
"    : row {"
"      : button { key=\"accept\"; label=\"Fix All\"; is_default=true; width=18; }"
"      : button { key=\"cancel\"; label=\"Cancel\"; is_cancel=true; width=18; }"
"    }"
"  }"
"}"
)
    (write-line line f))
  (close f)
  path)

(setq *fd3-all* nil)
(setq *fd3-visible* nil)
(setq *fd3-selected* 0)
(setq *fd3-show-unused* nil)
(setq *fd3-result* nil)

(defun fd3-build-visible (/ r)
  (setq *fd3-visible* nil)
  (foreach r *fd3-all*
    (if (or *fd3-show-unused* (> (nth 3 r) 0))
      (setq *fd3-visible* (append *fd3-visible* (list r)))))
  (if (>= *fd3-selected* (length *fd3-visible*))
    (setq *fd3-selected* 0)))

(defun fd3-row-line (r)
  (strcat
    (nth 0 r) "  |  "
    (if (fd3-empty-p (nth 1 r)) "<blank>" (nth 1 r))
    "  |  " (itoa (nth 3 r))
    "  |  -> " (nth 5 r)))

(defun fd3-refresh-list (/ r)
  (fd3-build-visible)
  (start_list "styles")
  (foreach r *fd3-visible*
    (add_list (fd3-row-line r)))
  (end_list)
  (if *fd3-visible*
    (set_tile "styles" (itoa *fd3-selected*))))

(defun fd3-load-popup (/ r choices choice idx)
  (if *fd3-visible*
    (progn
      (setq r (nth *fd3-selected* *fd3-visible*)
            choices (nth 4 r)
            choice (nth 5 r)
            idx (vl-position choice choices))
      (start_list "fonts")
      (mapcar 'add_list choices)
      (end_list)
      (set_tile "fonts" (itoa (if idx idx 0)))
      (set_tile "info"
        (strcat "Recommended: " (nth 6 r)
                "   |   Keep Current = do not change this style.")))
    (progn
      (start_list "fonts")
      (add_list "<No used suspect styles>")
      (end_list)
      (set_tile "fonts" "0")
      (set_tile "info" "No used suspect styles are currently shown."))))

(defun fd3-replace-row-in-all (old new)
  (setq *fd3-all* (subst new old *fd3-all*)))

(defun fd3-browse (/ p old choices trimmed new)
  (setq p (getfiled "Select replacement font" "" "ttf;otf;shx" 0))
  (if p
    (progn
      (setq old (nth *fd3-selected* *fd3-visible*)
            choices (nth 4 old))
      ; Remove <Browse...>, add chosen path, restore Browse at end.
      (setq trimmed (reverse (cdr (reverse choices)))
            choices (append trimmed (list p "<Browse...>"))
            new (subst choices (nth 4 old) old)
            new (subst p (nth 5 new) new))
      (fd3-replace-row-in-all old new)
      (fd3-refresh-list)
      (fd3-load-popup))))

(defun fd3-on-style ()
  (setq *fd3-selected* (atoi (get_tile "styles")))
  (fd3-load-popup))

(defun fd3-on-font (/ idx old choices choice new)
  (if *fd3-visible*
    (progn
      (setq idx (atoi (get_tile "fonts"))
            old (nth *fd3-selected* *fd3-visible*)
            choices (nth 4 old)
            choice (nth idx choices))
      (if (= choice "<Browse...>")
        (fd3-browse)
        (progn
          (setq new (subst choice (nth 5 old) old))
          (fd3-replace-row-in-all old new)
          (fd3-refresh-list)
          (fd3-load-popup))))))

(defun fd3-on-unused ()
  (setq *fd3-show-unused* (= (get_tile "unused") "1")
        *fd3-selected* 0)
  (fd3-refresh-list)
  (fd3-load-popup))

(defun fd3-dialog (qcount / tmp dclid ok usedcount unusedcount)
  (setq tmp (vl-filename-mktemp "font_doctor_v3_" nil ".dcl"))
  (fd3-write-dcl tmp)
  (setq dclid (load_dialog tmp)
        ok (and (> dclid 0) (new_dialog "fontdoctor3" dclid)))
  (if ok
    (progn
      (setq usedcount 0 unusedcount 0)
      (foreach r *fd3-all*
        (if (> (nth 3 r) 0)
          (setq usedcount (1+ usedcount))
          (setq unusedcount (1+ unusedcount))))
      (set_tile "summary"
        (strcat "Found " (itoa usedcount)
                " used suspect style(s)"
                (if (> unusedcount 0)
                  (strcat " + " (itoa unusedcount) " unused.")
                  ".")))
      (set_tile "unused" "0")
      (if (> qcount 0)
        (set_tile "warning"
          (strcat "WARNING: " (itoa qcount)
                  " object(s) contain '??'. Font repair cannot reconstruct characters already replaced by '?'."))
        (set_tile "warning" "No obvious '??' text-data loss detected."))
      (fd3-refresh-list)
      (fd3-load-popup)
      (action_tile "styles" "(fd3-on-style)")
      (action_tile "fonts" "(fd3-on-font)")
      (action_tile "unused" "(fd3-on-unused)")
      (action_tile "accept" "(setq *fd3-result* T)(done_dialog 1)")
      (action_tile "cancel" "(setq *fd3-result* nil)(done_dialog 0)")
      (start_dialog)))
  (if (> dclid 0) (unload_dialog dclid))
  (if (findfile tmp) (vl-file-delete tmp))
  *fd3-result*)

(defun c:FONTFIX (/ r style font big choices selected q fixed skipped failed failures)
  (setq *fd3-all* nil
        *fd3-visible* nil
        *fd3-selected* 0
        *fd3-show-unused* nil
        *fd3-result* nil)

  (foreach r (fd3-style-records)
    (setq style (nth 0 r)
          font (nth 1 r)
          big (nth 2 r))
    (if (fd3-suspect-p style font big)
      (progn
        (setq choices (fd3-choices style font)
              selected (fd3-default-choice style font choices))
        ; row = style font big count choices selected recommended
        (setq *fd3-all*
          (append *fd3-all*
            (list
              (list style font big
                    (fd3-count-style style)
                    choices selected
                    (fd3-recommended style font))))))))

  (if (null *fd3-all*)
    (alert "FONT DOCTOR V3\n\nNo suspect text styles found.\nNo changes made.")
    (progn
      (setq q (fd3-question-count))
      (if (fd3-dialog q)
        (progn
          (setq fixed 0 skipped 0 failed 0 failures nil)
          (command "_.UNDO" "_Begin")
          ; Default behavior: unused styles are not changed unless user explicitly
          ; displayed them. Used styles are always eligible.
          (foreach r *fd3-all*
            (setq selected (nth 5 r))
            (cond
              ((and (= (nth 3 r) 0) (not *fd3-show-unused*))
               (setq skipped (1+ skipped)))
              ((= selected "<Keep Current>")
               (setq skipped (1+ skipped)))
              ((fd3-write-style (nth 0 r) selected)
               (setq fixed (1+ fixed)))
              (T
               (setq failed (1+ failed)
                     failures (append failures (list (nth 0 r)))))))
          (command "_.UNDO" "_End")
          (command "_.REGENALL")
          (alert
            (strcat
              "FONT DOCTOR V3\n\nDone."
              "\nFixed: " (itoa fixed)
              "\nKept / unused: " (itoa skipped)
              "\nFailed: " (itoa failed)
              (if failures
                (strcat "\n\nFailed styles: "
                        (apply 'strcat
                          (cons (car failures)
                            (mapcar '(lambda (x) (strcat ", " x)) (cdr failures)))))
                "")
              (if (> q 0)
                (strcat "\n\nWarning: " (itoa q)
                        " object(s) contain '??'. Those characters may already be lost.")
                ""))))
        (princ "\nFONTFIX cancelled. No changes made."))))
  (princ))

(princ "\nFONT_DOCTOR V3 loaded. Type FONTFIX.")
(princ)
