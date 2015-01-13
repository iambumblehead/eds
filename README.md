# EDS: EDTS Development Suite

For use with EDTS, https://github.com/tjarvstrand/edts.

## Overview

EDS augments EDTS. [EDTS][1] is a tool for Erlang development. I've gradually added and fixed EDTS features here -loading these in emacs after EDTS.

Documentation may appear later. Consult EDS sources for now.

## Install

*.emacs*
```elisp
(load-file "~/Software/eds/eds-util.el")
(load-file "~/Software/eds/eds-proj.el")
(load-file "~/Software/eds/eds-shell.el")
(load-file "~/Software/eds/eds-stamp.el")
(load-file "~/Software/eds/eds-compile.el")

(eds-proj-set-opts
 "myproject"
 '((root-dir  . "~/path/to/myproject") 
   (log-dir   . "logs") 
   (src-dir   . "src")
   (lib-dirs  . ("deps/"))
   (otp-path  . "/usr/local/bin/erl")
   (st-author . "bumblehead <chris@bumblehead.com>")
   (st-copy   . "Ganimas LLC")))
```

## EDS Features:

  1. `eds-compile-proj`
     compile all otp-dependencies used by your application with edts. 

  2. `eds-shell-proj`
     start an edts-shell in the root directory of your application, using an app.config or sys.config file defined for your project. if no config file is defined that is ok.
     
  3. `eds-proj-set-opts`
     define projects using standard elisp. `eds-proj-set-opts` may be used multiple times to define multiple projects. Switch between defined projects using `eds-proj-set-name`.

  4. `eds-proj-*`
     functions named with the `eds-proj-*` pattern make project data easy to obtain and define any time.

  5. `eds-stamp.el*`
     if `st-copy` or `st-author` are defined, maintains a stamp at the top of your application src/ erl files each time one is saved. the stamp looks like this:
     ```erlang
     %% Filename: chat_app.erl  
     %% Timestamp: 2015.01.13-12:10:19 (last modified)
     %% Copyright: mycopy  
     %% Author(s): me <me@domain.com>       
     ```


[1]: https://github.com/tjarvstrand/edts "edts"
[2]: https://github.com/tjarvstrand/edts/issues/160#issuecomment-68508372 "redunderline"
[3]: https://github.com/iambumblehead/edtsredunderline "redunderline"
