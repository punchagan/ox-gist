# ox-gist.el --- Export Org mode buffers and subtrees to GitHub gists

Copyright (C) 2022 Puneeth Chaganti

* Author: Puneeth Chaganti <punchagan+emacs@muse-amuse.in>
* Created: 2022 March 08
* Version: 0.3
* Package-Requires: ((emacs "26.1") (gist "1.4.0") (s "1.12.0"))
* Keywords: org, lisp, gist, github
* URL: https://github.com/punchagan/org2gist/

This file is not part of GNU Emacs.

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

I often find myself wanting to share a subtree from or an entire Org mode
file publicly.  It's convenient to use GitHub gists for this, since GitHub
renders Org mode syntax almost correctly.  This package makes it easy to do
that.  The heavy lifting of creating and updating gists is delegated to
@defunkt's `` `gist.el' ``.

# Usage

`` `ox-gist' `` can be installed from Melpa.  Once you install and load the
package, you can use the `` `org-export-dispatch' `` function (usually bound to
`` `C-c C-e' ``) to "export" a buffer or a subtree to a GitHub gist.  The Org
export menu provides options to export as a public or private gist, and to
open the gist in a browser after publishing it.



