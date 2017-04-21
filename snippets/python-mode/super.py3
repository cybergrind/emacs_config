# -*- mode: snippet -*-
# name: super.py3
# key: su
# --
super().`(nth 1 (split-string (python-info-current-defun) "\\."))`($1)
$0