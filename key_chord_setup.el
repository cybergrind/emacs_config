(require 'key-chord)

(setq key-chord-two-keys-delay 0.05)
(setq key-chord-one-key-delay 0.2)

(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "JJ" 'switch-to-prev-buffer)
(key-chord-define-global "KK" 'switch-to-next-buffer)
(key-chord-define-global "fj" 'jump-to-register)
(key-chord-define-global "fy" 'point-to-register)
(key-chord-define-global "vm" 'bookmark-bmenu-list)

(key-chord-mode +1)

(provide 'key_chord_setup)
