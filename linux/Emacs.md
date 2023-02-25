# Emacs

### `it's a text editor by Emacs lisp language`

### Modify GUI settings

```emacs
(menu-bar-mode -1)           ;; hide the menu bar   -1 ==> false
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)


;;  change the Font settings

(custom-set-faces
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 264 :width normal)))))

```

<hr/>

```emacs
(setq variable-name variable-value)       ;; to set a variable
```
