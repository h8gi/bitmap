;;; bitmap.scm
(module bitmap
  (write-bmp make-bmp bmp-make-pixel)
  (import scheme chicken srfi-1 srfi-4 ports data-structures)
  (include "main.scm"))
