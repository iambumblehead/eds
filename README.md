# EDS: EDTS Development Suite

For use with EDTS, https://github.com/tjarvstrand/edts.

## Overview

EDS augments EDTS. [EDTS][1] is a tool for Erlang development. I decided to add and fix EDTS features through these files, loading them in emacs after EDTS.

Added User Features:

  1. `eds-compile-proj` _edts-compile's your otp dependencies_. Without this feature, EDTS red underlines dependency function calls as 'undefined'. With this EDTS will provide auto-completion and other goodness in places where dependency functions are used.

     EDTS apparently [comes with][1] functonality to read these definitions. It does [not work][2] for me.

  2. `eds-shell-proj` _start an edts-shell with an app.config or sys.config file_. Without this edts-shell is impractical for developing applications that require a config file.

  3. `eds-proj-set-opts` _define multiple projects using standard elisp_. Two projects are configured here:
  ```elisp
  (eds-proj-set-opts 
   "gani"
   '((root-dir  . "~/Software/gani") 
     (config    . "app.config")
     (log-dir   . "logs") 
     (src-dir   . "src")
     (lib-dirs  . ("deps/"))
     (otp-path  . "/usr/local/bin/erl")))
  (eds-proj-set-opts
   "edtsredunderline"
   '((root-dir  . "~/Software/edtsredunderline") 
     (log-dir   . "logs") 
     (src-dir   . "src")
     (lib-dirs  . ("deps/"))
     (otp-path  . "/usr/local/bin/erl")))
  ```
  EDS will reliably start a project shell for either of these projects no matter which file is opened in the buffer. Switching between projects is easy, `eds-proj-set-name`.

`eds-proj-*` functions simplify the project data management. Properties for any project are easily obtained/defined at once or individually.

## Install

*.emacs*
```elisp
(load-file "~/Software/eds/eds-util.el")
(load-file "~/Software/eds/eds-proj.el")
(load-file "~/Software/eds/eds-shell.el")
(load-file "~/Software/eds/eds-compile.el")

(eds-proj-set-opts
 "myproject"
 '((root-dir  . "~/path/to/myproject") 
   (log-dir   . "logs") 
   (src-dir   . "src")
   (lib-dirs  . ("deps/"))
   (otp-path  . "/usr/local/bin/erl")))
```


[1]: https://github.com/tjarvstrand/edts "edts"
[2]: https://github.com/tjarvstrand/edts/issues/160#issuecomment-68508372 "redunderline"
[3]: https://github.com/iambumblehead/edtsredunderline "redunderline"
