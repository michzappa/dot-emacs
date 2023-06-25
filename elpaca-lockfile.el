((elpaca :source "lockfile" :date
         (25752 44204 164369 380000)
         :recipe
         (:protocol https :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "38213ddf0a006f5a74b82f538c2da51f5818759c" :files
                    (:defaults
                     (:exclude "extensions"))
                    :build
                    (:not elpaca--activate-package)
                    :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date
                     (25752 44204 162641 528000)
                     :recipe
                     (:package "elpaca-use-package" :repo "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el")
                               :main "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info)
                               :protocol https :inherit t :depth 1 :ref "38213ddf0a006f5a74b82f538c2da51f5818759c"))
 (org :source "lockfile" :date
      (25752 44204 160871 925000)
      :recipe
      (:package "org" :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :pre-build
                (progn
                  (require 'elpaca-menu-org)
                  (elpaca-menu-org--build))
                :build
                (:not elpaca--generate-autoloads-async)
                :files
                (:defaults
                 ("etc/styles/" "etc/styles/*" "doc/*.texi"))
                :protocol https :inherit t :depth 1 :ref "18f003a16457e58cee5006291d7e8cb20950fb1b"))
 (org-contrib :source "lockfile" :date
              (25752 44204 159004 86000)
              :recipe
              (:package "org-contrib" :local-repo "org-contrib" :repo "https://git.sr.ht/~bzg/org-contrib" :files
                        (:defaults)
                        :protocol https :inherit t :depth 1 :ref "aed67d095de23bc45446777f7b8fb30b8e5c0c51"))
 (use-package :source "lockfile" :date
   (25752 44204 157142 244000)
   :recipe
   (:package "use-package" :fetcher github :repo "jwiegley/use-package" :files
             (:defaults
              (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el"))
             :protocol https :inherit t :depth 1 :ref "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (bind-key :source "lockfile" :date
           (25752 44204 155330 103000)
           :recipe
           (:package "bind-key" :fetcher github :repo "jwiegley/use-package" :files
                     ("bind-key.el")
                     :protocol https :inherit t :depth 1 :ref "a6e856418d2ebd053b34e0ab2fda328abeba731c"))
 (no-littering :source "lockfile" :date
               (25752 44204 153618 617000)
               :recipe
               (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "cc56893faaf769226ceae8836a426da583fab992"))
 (elfeed :source "lockfile" :date
         (25752 44204 151884 345000)
         :recipe
         (:package "elfeed" :repo "skeeto/elfeed" :fetcher github :files
                   (:defaults "README.md")
                   :protocol https :inherit t :depth 1 :ref "162d7d545ed41c27967d108c04aa31f5a61c8e16"))
 (apheleia :source "lockfile" :date
           (25752 44204 150106 254000)
           :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "f85b48e2eee0983acfe5b3a2684856358714ac6f"))
 (ws-butler :source "lockfile" :date
            (25752 44204 148395 756000)
            :recipe
            (:package "ws-butler" :fetcher github :repo "lewang/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e3a38d93e01014cd47bf5af4924459bd145fd7c4"))
 (scratch :source "lockfile" :date
          (25752 44204 146800 62000)
          :recipe
          (:package "scratch" :fetcher codeberg :repo "https://codeberg.org/emacs-weirdware/scratch" :files
                    ("scratch.el")
                    :protocol https :inherit t :depth 1 :ref "f000648c9663833a76a8de9b1e78c99a9d698e48"))
 (company :source "lockfile" :date
          (25752 44204 145154 491000)
          :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small" "doc/images/small/*.png"))
                    :protocol https :inherit t :depth 1 :ref "8a78f320019574bc35b5727f95b052b27918da20"))
 (macrostep :source "lockfile" :date
            (25752 44204 143559 420000)
            :recipe
            (:package "macrostep" :fetcher github :repo "emacsorphanage/macrostep" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "633586421e7fc14072cc1ca1655c1103b81a9093"))
 (sly :source "lockfile" :date
      (25752 44204 142024 401000)
      :recipe
      (:package "sly" :repo "joaotavora/sly" :fetcher github :files
                (:defaults "lib" "slynk" "contrib" "doc/images"
                           (:exclude "sly-autoloads.el"))
                :protocol https :inherit t :depth 1 :ref "df62abae73bd511885c9c7ec0ea7ea1469a00923"))
 (go-mode :source "lockfile" :date
          (25752 44204 140246 137000)
          :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
                    ("go-mode.el")
                    :protocol https :inherit t :depth 1 :ref "166dfb1e090233c4609a50c2ec9f57f113c1da72"))
 (haskell-mode :source "lockfile" :date
               (25752 44204 138512 525000)
               :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
                         (:defaults "NEWS" "logo.svg")
                         :protocol https :inherit t :depth 1 :ref "41c0cf61591279a22ac511f925c041c40969bdb8"))
 (auctex :source "lockfile" :date
         (25752 44204 136680 63000)
         :recipe
         (:package "auctex" :host github :files
                   ("*"
                    (:exclude ".git"))
                   :repo "emacs-straight/auctex" :protocol https :inherit t :depth 1 :ref "f60d3b907618c2cbb527e59e29821465d6750993"))
 (kotlin-mode :source "lockfile" :date
              (25752 44204 134765 646000)
              :recipe
              (:package "kotlin-mode" :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "fddd747e5b4736e8b27a147960f369b86179ddff"))
 (markdown-mode :source "lockfile" :date
                (25752 44204 133068 16000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "f3ee31ffc28b3d8e86da2208c87eac75fd6e6eae"))
 (nix-mode :source "lockfile" :date
           (25752 44204 131264 434000)
           :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :protocol https :inherit t :depth 1 :ref "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (nixos-options :source "lockfile" :date
                (25752 44204 129613 24000)
                :recipe
                (:package "nixos-options" :fetcher github :repo "travisbhartwell/nix-emacs" :files
                          ("nixos-options.el")
                          :protocol https :inherit t :depth 1 :ref "053a2d5110ce05b7f99bcc2ac4804b70cbe87916"))
 (tuareg :source "lockfile" :date
         (25752 44204 127938 390000)
         :recipe
         (:package "tuareg" :fetcher github :repo "ocaml/tuareg" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "e5d792009237c952c44cd9c59873dee9719f1280"))
 (racket-mode :source "lockfile" :date
              (25752 44204 126196 207000)
              :recipe
              (:package "racket-mode" :fetcher github :repo "greghendershott/racket-mode" :files
                        (:defaults "*.rkt"
                                   ("racket" "racket/*")
                                   (:exclude "racket/example/*" "racket/test/*"))
                        :protocol https :inherit t :depth 1 :ref "c6dbe023c688ad1845e1f8ee250f5fa2cf2b8b01"))
 (rust-mode :source "lockfile" :date
            (25752 44204 124458 442000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e443ccf2884028d3b6cc550ff20e7c92dadccb68"))
 (yaml-mode :source "lockfile" :date
            (25752 44204 122650 428000)
            :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "b153150e0e77b4ec462d741cdb16956c6ae270d6"))
 (simple-httpd :source "lockfile" :date
               (25752 44204 120911 219000)
               :recipe
               (:package "simple-httpd" :repo "skeeto/emacs-web-server" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "6260232da8b53c4e214bc5e427ec00c3ebdd9869"))
 (web-mode :source "lockfile" :date
           (25752 44204 119104 325000)
           :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "57856ba64b9382811b35df0d9ab0a24aede0c1f0"))
 (kbd-mode :source "lockfile" :date
           (25752 44204 117416 897000)
           :recipe
           (:protocol https :inherit t :depth 1 :host github :repo "kmonad/kbd-mode" :package "kbd-mode" :ref "96178a43d3c9ea3167362513fe4c3fdeb7074e9f"))
 (clipetty :source "lockfile" :date
           (25752 44204 115647 136000)
           :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (ctrlf :source "lockfile" :date
        (25752 44204 113821 434000)
        :recipe
        (:package "ctrlf" :fetcher github :repo "radian-software/ctrlf" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "9b4cf6c79a961f2bfbb949805aa300fcf1eb40a6"))
 (undo-tree :source "lockfile" :date
            (25752 44204 111960 95000)
            :recipe
            (:package "undo-tree" :host github :files
                      ("*"
                       (:exclude ".git"))
                      :repo "emacs-straight/undo-tree" :protocol https :inherit t :depth 1 :ref "16f4121032d09ef44b3d7d02c4d02c3c2f18041f"))
 (dumb-jump :source "lockfile" :date
            (25752 44204 110094 545000)
            :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "90130b85fec948acb7c6f623d2c3982533955bf6"))
 (avy :source "lockfile" :date
      (25752 44204 108139 303000)
      :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (multiple-cursors :source "lockfile" :date
                   (25752 44204 106201 422000)
                   :recipe
                   (:package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :protocol https :inherit t :depth 1 :ref "6956e8e12ee191d7c80d042ae8ff495286fcbe38"))
 (puni :source "lockfile" :date
       (25752 44204 104257 554000)
       :recipe
       (:package "puni" :repo "AmaiKinono/puni" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "a39a4ecac7279bed1a150a895bbc80baa7272888"))
 (expand-region :source "lockfile" :date
                (25752 44204 102263 704000)
                :recipe
                (:package "expand-region" :repo "magnars/expand-region.el" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "b70feaa644310dc2d599dc277cd20a1f2b6446ac"))
 (marginalia :source "lockfile" :date
             (25752 44204 100248 791000)
             :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "ae454a2aa0c5d85b5e151938b310e7d16538157d"))
 (vertico :source "lockfile" :date
          (25752 44204 98275 111000)
          :recipe
          (:package "vertico" :host github :files
                    ("*"
                     (:exclude ".git"))
                    :repo "emacs-straight/vertico" :protocol https :inherit t :depth 1 :ref "17d7cf79888114c1cbea89cd8e58a3bba986f369"))
 (orderless :source "lockfile" :date
            (25752 44204 96264 436000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
 (consult :source "lockfile" :date
          (25752 44204 94387 471000)
          :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "0e90f516c8cae705709a24ea8071a0c2400360d8"))
 (helpful :source "lockfile" :date
          (25752 44204 92493 521000)
          :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "32cb28b50b3366ad35e2cb936367268ddeec745f"))
 (which-key :source "lockfile" :date
            (25752 44204 90723 768000)
            :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "ee6f0637f75ded903653b7a300a8588e3a8427f7"))
 (org-drill :source "lockfile" :date
            (25752 44204 88972 324000)
            :recipe
            (:package "org-drill" :fetcher gitlab :repo "phillord/org-drill" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "bf8fe812d44a3ce3e84361fb39b8ef28ca10fd0c"))
 (ox-hugo :source "lockfile" :date
          (25752 44204 87047 455000)
          :recipe
          (:package "ox-hugo" :fetcher github :repo "kaushalmodi/ox-hugo" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "d6aefe7165bd842d7826931eb7f0e700e1bdb0db"))
 (org-bullets :source "lockfile" :date
              (25752 44204 85185 632000)
              :recipe
              (:package "org-bullets" :fetcher github :repo "integral-dw/org-bullets" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "767f55feb58b840a5a04eabfc3fbbf0d257c4792"))
 (magit :source "lockfile" :date
        (25752 44204 83182 286000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
                   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "24f64fd4f8ed4a4a302fd9227febad63507d7287"))
 (forge :source "lockfile" :date
        (25752 44204 81334 291000)
        :recipe
        (:package "forge" :fetcher github :repo "magit/forge" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "ec68fcd778f6b3dc6100498aea790457d2fc98f6"))
 (exec-path-from-shell :source "lockfile" :date
                       (25752 44204 79331 434000)
                       :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :protocol https :inherit t :depth 1 :ref "ddd24dc823de9a94435b4d8ea7569161657f31e2"))
 (direnv :source "lockfile" :date
         (25752 44204 77295 302000)
         :recipe
         (:package "direnv" :fetcher github :repo "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "268536f564b7eba99264a89a9149268eb4bc67ac"))
 (auto-compile :source "lockfile" :date
               (25752 44204 75305 691000)
               :recipe
               (:package "auto-compile" :repo "emacscollective/auto-compile" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "3a255903643227c0db10ca2371c33ba9e8ec9924"))
 (all-the-icons :source "lockfile" :date
                (25752 44204 73232 989000)
                :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github :files
                          (:defaults "data")
                          :protocol https :inherit t :depth 1 :ref "f491f39c21336d354e85bdb4cca281e0a0c2f880"))
 (all-the-icons-completion :source "lockfile" :date
                           (25752 44204 71326 886000)
                           :recipe
                           (:package "all-the-icons-completion" :repo "iyefrat/all-the-icons-completion" :fetcher github :files
                                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                     :protocol https :inherit t :depth 1 :ref "8eb3e410d63f5d0657b41829e7898793e81f31c0"))
 (dashboard :source "lockfile" :date
            (25752 44204 69443 584000)
            :recipe
            (:package "dashboard" :fetcher github :repo "emacs-dashboard/emacs-dashboard" :files
                      (:defaults "banners")
                      :protocol https :inherit t :depth 1 :ref "34a0076f01a729b4aae16947fa0d0e130cafedfd"))
 (default-text-scale :source "lockfile" :date
   (25752 44204 67470 742000)
   :recipe
   (:package "default-text-scale" :fetcher github :repo "purcell/default-text-scale" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref "bfc0987c37e93742255d3b23d86c17096fda8e7e"))
 (ef-themes :source "lockfile" :date
            (25752 44204 65293 213000)
            :recipe
            (:package "ef-themes" :host github :files
                      ("*"
                       (:exclude ".git"))
                      :repo "emacs-straight/ef-themes" :protocol https :inherit t :depth 1 :ref "9710ef3c149ab4e962a9cb7a8566fb26e4f780bb"))
 (modus-themes :source "lockfile" :date
               (25752 44204 63283 395000)
               :recipe
               (:package "modus-themes" :fetcher sourcehut :repo "protesilaos/modus-themes" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "99b23d69836d10c76612cc5549faba4116ed85db"))
 (minions :source "lockfile" :date
          (25752 44204 61366 207000)
          :recipe
          (:package "minions" :fetcher github :repo "tarsius/minions" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "62948a4a2951dab0716977421bfe0a87ea2583c5"))
 (hl-todo :source "lockfile" :date
          (25752 44204 59353 342000)
          :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "b27cddf7373408681cc949c8ef829f87a01ed3f3"))
 (rainbow-delimiters :source "lockfile" :date
                     (25752 44204 57416 985000)
                     :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :protocol https :inherit t :depth 1 :ref "a32b39bdfe6c61c322c37226d66e1b6d4f107ed0"))
 (rainbow-mode :source "lockfile" :date
               (25752 44204 55542 454000)
               :recipe
               (:package "rainbow-mode" :host github :files
                         ("*"
                          (:exclude ".git"))
                         :repo "emacs-straight/rainbow-mode" :protocol https :inherit t :depth 1 :ref "8e96388fb4d616a9dde23e712bad0d9cd048fbf0"))
 (git-gutter :source "lockfile" :date
             (25752 44204 53661 928000)
             :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "6b3e6704d83ccff42985940879bb14082a672fb8"))
 (atomic-chrome :source "lockfile" :date
                (25752 44204 51841 50000)
                :recipe
                (:package "atomic-chrome" :repo "alpha22jp/atomic-chrome" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "f1b077be7e414f457191d72dcf5eedb4371f9309"))
 (nov :source "lockfile" :date
      (25752 44204 50126 110000)
      :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "58c35e677e11f5c04a702b42ac753c80c8955089"))
 (ledger-mode :source "lockfile" :date
              (25752 44204 48155 466000)
              :recipe
              (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode" :files
                        ("ledger*.el")
                        :old-names
                        (ldg-mode)
                        :protocol https :inherit t :depth 1 :ref "ca59ac2504fa5616cf0bfc1c95c5cea2a828ade4"))
 (compat :source "lockfile" :date
         (25752 44204 46247 627000)
         :recipe
         (:package "compat" :host github :files
                   ("*"
                    (:exclude ".git"))
                   :repo "emacs-straight/compat" :protocol https :inherit t :depth 1 :ref "e07c0f29d45a73cc0bdf9423780979978c1d9d22"))
 (magit-section :source "lockfile" :date
                (25752 44204 44443 169000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi")
                          :protocol https :inherit t :depth 1 :ref "24f64fd4f8ed4a4a302fd9227febad63507d7287"))
 (dash :source "lockfile" :date
       (25752 44204 42693 842000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi")
                 :protocol https :inherit t :depth 1 :ref "d5182da04ca54c026ea0bf381f2c1642a30e2686"))
 (caml :source "lockfile" :date
       (25752 44204 40845 486000)
       :recipe
       (:package "caml" :repo "ocaml/caml-mode" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "959a5a27bfdbaa43a9ff99be136d62e0798f5e01"))
 (queue :source "lockfile" :date
        (25752 44204 38975 512000)
        :recipe
        (:package "queue" :host github :files
                  ("*"
                   (:exclude ".git"))
                  :repo "emacs-straight/queue" :protocol https :inherit t :depth 1 :ref "130c2d656cd5d7376552272fab9e50a7c37d0c4a"))
 (s :source "lockfile" :date
    (25752 44204 37102 90000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (popup :source "lockfile" :date
        (25752 44204 35178 490000)
        :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "69efb517f3b8ba8ed82dfb4e39b74b325bc98a59"))
 (f :source "lockfile" :date
    (25752 44204 33265 792000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "af7d37c619010b576fd22b50c62c71ff33093f3c"))
 (elisp-refs :source "lockfile" :date
             (25752 44204 31282 657000)
             :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults
                        (:exclude "elisp-refs-bench.el"))
                       :protocol https :inherit t :depth 1 :ref "bf3cca8f74065b1b31036f461e3a093b162311bd"))
 (persist :source "lockfile" :date
          (25752 44204 29284 893000)
          :recipe
          (:package "persist" :host github :files
                    ("*"
                     (:exclude ".git"))
                    :repo "emacs-straight/persist" :protocol https :inherit t :depth 1 :ref "c10835478d9f916534a07fad0174d497adf85729"))
 (tomelr :source "lockfile" :date
         (25752 44204 27277 984000)
         :recipe
         (:package "tomelr" :host github :files
                   ("*"
                    (:exclude ".git"))
                   :repo "emacs-straight/tomelr" :protocol https :inherit t :depth 1 :ref "57cb24df521031a6d02f61091db82d292e4175df"))
 (git-commit :source "lockfile" :date
             (25752 44204 25234 768000)
             :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
                       ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
                       :old-names
                       (git-commit-mode)
                       :protocol https :inherit t :depth 1 :ref "24f64fd4f8ed4a4a302fd9227febad63507d7287"))
 (with-editor :source "lockfile" :date
   (25752 44204 23320 233000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref "9e437353ee817b8e6a9ffce53e37fe5a6fcb4294"))
 (closql :source "lockfile" :date
         (25752 44204 21477 18000)
         :recipe
         (:package "closql" :fetcher github :repo "magit/closql" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "85ac7b8a894a4e259439d79eb6bd6f5129770905"))
 (emacsql :source "lockfile" :date
          (25752 44204 19529 367000)
          :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
                    (:defaults "sqlite")
                    :protocol https :inherit t :depth 1 :ref "64012261f65fcdd7ea137d1973ef051af1dced42"))
 (ghub :source "lockfile" :date
       (25752 44204 17624 217000)
       :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "0fd648f0529f34b5da441d54c9dbff7810477d12"))
 (let-alist :source "lockfile" :date
            (25752 44204 15604 146000)
            :recipe
            (:package "let-alist" :host github :files
                      ("*"
                       (:exclude ".git"))
                      :repo "emacs-straight/let-alist" :protocol https :inherit t :depth 1 :ref "021fc10df2e44faba4728d849ee767cf890aa51a"))
 (yaml :source "lockfile" :date
       (25752 44204 13380 982000)
       :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "a19fbf948a945571300e5a20ff1dbfa6ecfa0d16"))
 (treepy :source "lockfile" :date
         (25752 44204 11347 164000)
         :recipe
         (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "298c49b48f36b230224421ff2f168734f74fce15"))
 (websocket :source "lockfile" :date
            (25752 44204 9250 829000)
            :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "1a08093b122d8cf20366a1cba5faddf7a53d08ed"))
 (esxml :source "lockfile" :date
        (25752 44204 7062 115000)
        :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el")
                  :protocol https :inherit t :depth 1 :ref "225693096a587492d76bf696d1f0c25c61f7d531"))
 (kv :source "lockfile" :date
     (25752 44204 4957 468000)
     :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "721148475bce38a70e0b678ba8aa923652e8900e"))
 (elfeed-org :source "lockfile" :date
             (25752 44204 2802 516000)
             :recipe
             (:package "elfeed-org" :repo "remyhonig/elfeed-org" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "3242ec0519800a58f20480c8a6e3b3337d137084")))
