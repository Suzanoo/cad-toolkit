(defun c:REVITCLEAN ( / ss i ent obj objname deltypes)
  (vl-load-com)

  (command "_.AUDIT" "_Y")
  (command "_.-PURGE" "_ALL" "*" "_N")
  (command "_.-PURGE" "_REGAPPS" "*" "_N")

  (setq deltypes
    '(
      "AcDbDimension"
      "AcDbHatch"
      "AcDbLeader"
      "AcDbMLeader"
      "AcDbTable"
      "AcDbWipeout"
      "AcDbRasterImage"
    )
  )

  (setq ss (ssget "_X" '((410 . "Model"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq objname (vla-get-ObjectName obj))

        (if (member objname deltypes)
          (entdel ent)
        )

        (setq i (1+ i))
      )
    )
  )

  (command "_.OVERKILL" "_ALL" "")
  (command "_.-PURGE" "_ALL" "*" "_N")
  (command "_.-PURGE" "_REGAPPS" "*" "_N")
  (command "_.ZOOM" "_E")
  (command "_.QSAVE")

  (princ "\nREVIT CLEAN DONE.")
  (princ)
)