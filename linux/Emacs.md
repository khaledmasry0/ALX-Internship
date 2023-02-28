<h1 align="center"> Emacs </h1>

### `it's a text editor by Emacs lisp language`

### [Emacs Guide](https://systemcrafters.net/emacs-essentials/absolute-beginners-guide-to-emacs/)

### [Keys and Commands](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)

### [Emacs Repo](https://github.com/daviwil/dotfiles/blob/master/Emacs.org)

### [Emacs tour guide](https://www.gnu.org/software/emacs/tour/)

<hr/>

```emacs
install emacs : pacman -S mingw-w64-x86_64-emacs   ;; if you use MSYS2
choco install emacs            ;; using Chocolatey
```

### Modify GUI settings

#### you can put any code in .Emacs and [C-x e] ==> ctrl x then e

```emacs
(menu-bar-mode -1) ;; إخفاء شريط القوائم
(tool-bar-mode -1) ;; إخفاء شريط الأدوات
(scroll-bar-mode -1) ;; إخفاء شريط التمرير
(blink-cursor-mode -1) ;; إيقاف وميض مؤشر الكتابة
(tooltip-mode -1) ;; إخفاء تلمحيات الماوس


;;  تغيير الخط
;; القيمة 264 مناسبة للشاشات ذات الدقة العالية، استخدم نصف هذه القيمة لأنواع الشاشات الأخرى
(custom-set-faces
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 264 :width normal)))))

;; النسخ الاحتياطي
(setq make-backup-files t) ;; احفظ بنسخ احتياطية للملفات التي يجري تحريرها
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backup-files/")))) ;; ضع جميع هذه الملفات في هذا المسار


;; إيقاف خاصية الحفظ التلقائي للملفات، غير منصوح به!
(setq auto-save-default nil)

;; مضاعفة السطر الحالي أو المنطقة المُحددة
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region) ;; اختصار لوحة المفاتيح لتفعيل هذه الدالة



;; التبديل بين التقسيم العمودي والأفقي ضمن إطار ايماكس

(defun toggle-window-split ()
(interactive)
(if (= (count-windows) 2)
(let* ((this-win-buffer (window-buffer))
(next-win-buffer (window-buffer (next-window)))
(this-win-edges (window-edges (selected-window)))
(next-win-edges (window-edges (next-window)))
(this-win-2nd (not (and (<= (car this-win-edges)
(car next-win-edges))
(<= (cadr this-win-edges)
(cadr next-win-edges)))))
(splitter
(if (= (car this-win-edges)
(car (window-edges (next-window))))
'split-window-horizontally
'split-window-vertically)))
(delete-other-windows)
(let ((first-win (selected-window)))
(funcall splitter)
(if this-win-2nd (other-window 1))
(set-window-buffer (selected-window) this-win-buffer)
(set-window-buffer (next-window) next-win-buffer)
(select-window first-win)
(if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split) ;; اختصار لوحة المفاتيح لتفعيل هذه الدالة

;; ========================================================================

(global-hl-line-mode 1) ;; إبراز السطر الحالي
(electric-pair-mode 1) ;; إغلاق تلقائي للأقواس
(show-paren-mode 1) ;; إبراز الأقواس عند الوقوف عليها
(setq isearch-allow-scroll t) ;; السماح بالسكرول دون الخروج من عملية البحث الحالية
(global-visual-line-mode 1) ;; الأسطر هي الأسطر المرئية، يُشبه خيار إلتفاف الأسطر في باقي المحررات
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; تفعيل إظهار أرقام الأسطر تلقائيا في الملفات البرمجية، فقط مع ايماكس 26
(add-hook 'prog-mode-hook #'linum-mode) ;; مشابه للأمر السابق ويعمل على ايماكس 25

https://melpa.org/#/getting-started  ;; for more packages     .emacs إلى ملف melpa أولا ينبغي إضافة متجر

;; استخدام نظام البحث البديل آيفي
;; ivy + ivy-rich + smex + flx
(require 'swiper)
(require 'smex)
(require 'flx)
(require 'ivy-rich)
(ivy-mode 1)
(ivy-rich-mode 1)
(setq ivy-virtual-abbreviate 'full)
(setq ivy-rich-path-style 'abbrev)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "H-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
      (t . ivy--regex-fuzzy)))
(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer 'ivy-switch-buffer-rich-transformer)

 ;; حذف النص المُحدد عند إدراج نص جديد
 (delete-selection-mode +1)

;; إظهار رقم العمود الحالي في شريط الحالة
(column-number-mode +1)

;; تغيير الخط المسؤول عن عرض النصوص العربية فقط
(set-fontset-font "fontset-default"
		     'arabic
		     (font-spec :family "Kawkab Mono" :size 30))


(setq mouse-wheel-scroll-amount '(1))  ;; قم بالتمرير سطر واحد في كل مرة
(setq mouse-wheel-progressive-speed nil) ;; لا تمرر أسطر كثيرة عند تسريع التمرير

;; استخدام Avy
;; https://github.com/abo-abo/avy

(global-set-key (kbd "C-:") 'avy-goto-char) ;; القفز إلى محرف ما
(global-set-key (kbd "C-'") 'avy-goto-char-2) ;; القفز إلى محرف باستخدام محرفين
(global-set-key (kbd "M-g f") 'avy-goto-line) ;; القفز إلى سطر
(global-set-key (kbd "M-g w") 'avy-goto-word-1) ;; القفز إلى كلمة باستخدام حرفها الأول
(global-set-key (kbd "M-g e") 'avy-goto-word-0) ;; القفز إلى أي كلمة من كلمات البفر

```

<hr/>

```emacs
(setq variable-name variable-value)       ;; to set a variable
```

<hr/>

# Emacs Commands

### Basics

```emacs
C = Control
M = Meta = Alt|Esc

C-x C-f "find" file i.e. open/create a file in buffer
C-x C-s save the file
C-x C-w write the text to an alternate name
C-x C-v find alternate file
C-x i insert file at cursor position
C-x b create/switch buffers
C-x C-b show buffer list
C-x k kill buffer
C-z suspend emacs
C-X C-c close down emacs
```

### Basic movements

```emacs
C-f forward char
C-b backward char
C-p previous line
C-n next line
M-f forward one word
M-b backward one word
C-a beginning of line
C-e end of line
C-v one page up
M-v scroll down one page
M-< beginning of text
M-> end of text
```

### Editing

```emacs
M-n repeat the following command n times
C-u repeat the following command 4 times
C-u n repeat n times
C-d delete a char
M-d delete word
M-Del delete word backwards
C-k kill line

C-Space Set beginning mark (for region marking for example)
C-W "kill" (delete) the marked region region
M-W copy the marked region
C-y "yank" (paste) the copied/killed region/line
M-y yank earlier text (cycle through kill buffer)
C-x C-x exchange cursor and mark

C-t transpose two chars
M-t transpose two words
C-x C-t transpose lines
M-u make letters uppercase in word from cursor position to end
M-c simply make first letter in word uppercase
M-l opposite to M-u
```

### Important

```emacs
C-g quit the running/entered command
C-x u undo previous action
M-x revert-buffer RETURN (insert like this) undo all changes since last save
M-x recover-file RETURN Recover text from an autosave-file
M-x recover-session RETURN if you edited several files
```

### Online help

```emacs
C-h c which command does this keystroke invoke
C-h k which command does this keystroke invoke and what does it do?
C-h l what were my last 100 typed keys
C-h w what key-combo does this command have?
C-h f what does this function do
C-h v what's this variable and what is it's value
C-h b show all keycommands for this buffer
C-h t start the emacs tutorial
C-h i start the info reader
C-h C-k start up info reader and go to a certain key-combo point
C-h F show the emacs FAQ
C-h p show infos about the Elisp package on this machine
```

### Search/Replace

```emacs
C-s Search forward
C-r search backward
C-g return to where search started (if you are still in search mode)
M-% query replace
Space or y replace this occurence
Del or n don't replace
. only replace this and exit (replace)
, replace and pause (resume with Space or y)
! replace all following occurences
^ back to previous match
RETURN or q quit replace
```

### Search/Replace with regular expressions

```emacs
Characters to use in regular expressions:
^ beginning of line
$ end of line
. single char
.* group or null of chars
\< beginning of a word
\> end of a word
[] every char inside the backets (for example [a-z] means every small letter)

M C-s RETURN search for regular expression forward
M C-r RETURN search for regular expression backward
M C-s incremental search
C-s repeat incremental search
M C-r incremental search backwards
C-r repeat backwards
M-x query-replace-regexp search and replace
```

### Window-Commands

```emacs
C-x 2 split window vertically
C-x o change to other window
C-x 0 delete window
C-x 1 close all windows except the one the cursors in
C-x ^ enlarge window
M-x shrink-window command says it ;
M C-v scroll other window
C-x 4 f find file in other window
C-x 4 o change to other window
C-x 4 0 kill buffer and window
C-x 5 2 make new frame
C-x 5 f find file in other frame
C-x 5 o change to other frame
C-x 5 0 close this frame
```

### Bookmark commands

```emacs
C-x r m set a bookmark at current cursor pos
C-x r b jump to bookmark
M-x bookmark-rename says it
M-x bookmark-delete "
M-x bookmark-save "
C-x r l list bookmarks
d mark bookmark for deletion
r rename bookmark
s save all listed bookmarks
f show bookmark the cursor is over
m mark bookmarks to be shown in multiple window
v show marked bookmarks (or the one the cursor is over)
t toggle listing of the corresponding paths
w " path to this file
x delete marked bookmarks
Del ?
q quit bookmark list


M-x bookmark-write write all bookmarks in given file
M-x bookmark-load load bookmark from given file
```

### Shell

```emacs
M-x shell starts shell modus
C-c C-c same as C-c under unix (stop running job)
C-d delete char forward
C-c C-d Send EOF
C-c C-z suspend job (C-z under unix)
M-p show previous commands
```

### DIRectory EDitor (dired)

```emacs
C-x d start up dired
C (large C) copy
d mark for erase
D delete right away
e or f open file or directory
g reread directory structure from file
G change group permissions (chgrp)
k delete line from listing on screen (don't actually delete)
m mark with *
n move to next line
o open file in other window and go there
C-o open file in other window but don't change there
P print file
q quit dired
Q do query-replace in marked files
R rename file
u remove mark
v view file content
x delete files marked with D
z compress file
M-Del remove all marks (whatever kind)
~ mark backup files (name~ files) for deletion
# mark auto-save files (#name#) for deletion
*/ mark directory with * (C-u * removes that mark again)
= compare this file with marked file
M-= compare this file with it's backup file
! apply shell command to this file
M-} change to the next file marked with * od D
M-{ " previous "
% d mark files described through regular expression for deletion
% m " (with *)
+ create directory
> changed to next dir
< change to previous dir
s toggle between sorting by name or date
```

### Telnet

```emacs
M-x telnet starts up telnet-modus
C-d either delete char or send EOF
C-c C-c stop running job (similar to C-c under unix)
C-c C-d send EOF
C-c C-o clear output of last command
C-c C-z suspend execution of command
C-c C-u kill line backwards
M-p recall previous command
```

### Text

```emacs
;;Works only in text mode
M-s center line
M-S center paragraph
M-x center-region name says
```

### Macro-commands

```emacs
C-x ( start macro definition
C-x ) end of macro definition
C-x e execute last definied macro
M-n C-x e execute last defined macro n times
M-x name-last-kbd-macro give name to macro (for saving)
M-x insert-keyboard-macro save named macro into file
M-x load-file load macro
M-x macroname execute macroname
```

### Programming

```emacs
M C-\ indent region between cursor and mark
M-m move to first (non-space) char in this line
M-^ attach this line to previous
M-; formatize and indent comment
C, C++ and Java Modes
M-a beginning of statement
M-e end of statement
M C-a beginning of function
M C-e end of function
C-c RETURN Set cursor to beginning of function and mark at the end
C-c C-q indent the whole function according to indention style
C-c C-a toggle modus in which after electric signs (like {}:';./*) emacs does the indention
C-c C-d toggle auto hungry mode in which emacs deletes groups of spaces with one del-press
C-c C-u go to beginning of this preprocessor statement
C-c C-c comment out marked area
More general (I guess)
M-x outline-minor-mode collapses function definitions in a file to a mere {...}
M-x show-subtree If you are in one of the collapsed functions, this un-collapses it
In order to achive some of the feats coming up now you have to run etags *.c *.h *.cpp (or what ever ending you source files have) in the source directory
M-. (Thats Meta dot) If you are in a function call, this will take you to it's definition
M-x tags-search ENTER Searches through all you etaged
M-, (Meta comma) jumps to the next occurence for tags-search
M-x tags-query-replace yum. This lets you replace some text in all the tagged files
```

### GDB (Debugger)

```emacs
M-x gdb starts up gdm in an extra window

Version Control
C-x v d show all registered files in this dir
C-x v = show diff between versions
C-x v u remove all changes since last checkin
C-x v ~ show certain version in different window
C-x v l print log
C-x v i mark file for version control add
C-x v h insert version control header into file
C-x v r check out named snapshot
C-x v s create named snapshot
C-x v a create changelog file in gnu-style
```

<h1 align="center"> Vim </h1>

```emacs
vi file_name     ;; open the text file
<esc>            ;; to able to run the command
:wq              ;; to save and exit
```
