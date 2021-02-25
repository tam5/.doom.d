;;; +icons.el -*- lexical-binding: t; -*-

(setq all-the-icons-icon-alist
  '(
    ;; Meta
    ("\\.tags"          all-the-icons-octicon "tag"                     :height 1.0 :v-adjust -0.05 :face all-the-icons-blue)
    ("^TAGS$"           all-the-icons-octicon "tag"                     :height 1.0 :v-adjust -0.05 :face all-the-icons-blue)
    ("\\.log$"          all-the-icons-octicon "bug"                     :face all-the-icons-red)

    ;;
    ("\\.key$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.pem$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.p12$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.crt$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.pub$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.gpg$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lpink)

    ("^TODO$"           all-the-icons-octicon "checklist"               :v-adjust 0.0 :face all-the-icons-lyellow)
    ("^LICENSE$"        all-the-icons-octicon "key"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-lpink)
    ("^readme"          all-the-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-lcyan)

    ("\\.fish$"         all-the-icons-octicon "terminal"             :face all-the-icons-purple)
    ("\\.zsh$"          all-the-icons-octicon "terminal"             :face all-the-icons-purple)
    ("\\.sh$"           all-the-icons-octicon "terminal"             :face all-the-icons-purple)

    ;; Config
    ("\\.node$"         all-the-icons-alltheicon "nodejs"               :height 1.0  :face all-the-icons-green)
    ("\\.babelrc$"      all-the-icons-fileicon "babel"                  :height 0.65 :face all-the-icons-lyellow)
    ("\\.bashrc$"       all-the-icons-alltheicon "script"               :v-adjust -0.05 :height 0.65  :face all-the-icons-dpurple)
    ("\\.bowerrc$"      all-the-icons-alltheicon "bower"                :height 0.85 :v-adjust -0.05 :face all-the-icons-dorange)
    ("\\.eslintignore$" all-the-icons-fileicon "eslint"                 :height 0.9  :face all-the-icons-purple)
    ("\\.eslint"        all-the-icons-fileicon "eslint"                 :height 0.9  :face all-the-icons-purple)
    ("\\.eslintrc$"     all-the-icons-fileicon "eslint"                 :height 0.9  :face all-the-icons-purple)
    ("^\\.?eslintrc\\.?json" all-the-icons-fileicon "eslint"               :height 0.9  :face all-the-icons-purple)
    ("\\.git"           all-the-icons-alltheicon "git"                  :v-adjust 0.0 :height 0.65  :face all-the-icons-dorange)
    ("\\.gitignore"     all-the-icons-alltheicon "git"                  :v-adjust 0.0 :height 0.65  :face all-the-icons-dorange)
    ("\\.gitattributes" all-the-icons-alltheicon "git"                  :v-adjust 0.0 :height 0.65  :face all-the-icons-dorange)
    ("nginx"            all-the-icons-fileicon "nginx"                  :height 0.9  :face all-the-icons-dgreen)
    ("apache"           all-the-icons-alltheicon "apache"               :height 0.9  :face all-the-icons-dgreen)
    ("^Makefile$"       all-the-icons-fileicon "gnu"                    :face all-the-icons-dorange)
    ("\\.mk$"           all-the-icons-fileicon "gnu"                    :face all-the-icons-dorange)

    ("\\.dockerignore$" all-the-icons-fileicon "dockerfile"             :height 1.0 :v-adjust -0.35 :face all-the-icons-dblue)
    ("^\\.?Dockerfile"  all-the-icons-fileicon "dockerfile"             :height 1.0 :v-adjust -0.35 :face all-the-icons-dblue)
    ("^Brewfile$"       all-the-icons-faicon "beer"                     :face all-the-icons-lsilver)
    ("\\.npmignore$"    all-the-icons-fileicon "npm"                    :face all-the-icons-dred)
    ("^yarn\\.lock"     all-the-icons-fileicon "yarn"                   :face all-the-icons-blue-alt)

    ("\\.xml$"          all-the-icons-fileicon "MJML"              :height 0.65 :face all-the-icons-dorange)



    ("^serverless\\.yml$" all-the-icons-faicon "bolt"                   :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.ini$"          all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.[jc]son$"      all-the-icons-fileicon "jsonld"                :v-adjust 0.0 :face all-the-icons-yellow)
    ("^bower.json$"     all-the-icons-alltheicon "bower"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lorange)
    ("^package.json$"   all-the-icons-fileicon "npm-old"                    :face all-the-icons-dred)
    ("^package.lock.json$" all-the-icons-fileicon "npm-old"                 :face all-the-icons-dred)
    ("^stack.*.json$"   all-the-icons-alltheicon "aws"                  :face all-the-icons-orange)
    ("\\.ya?ml$"        all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-dyellow)
    ("\\.toml$"         all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-dyellow)

    ("\\.pkg$"          all-the-icons-octicon "package"                 :v-adjust 0.0 :face all-the-icons-dsilver)
    ("\\.rpm$"          all-the-icons-octicon "package"                 :v-adjust 0.0 :face all-the-icons-dsilver)

    ("\\.elc$"          all-the-icons-octicon "file-binary"             :v-adjust 0.0 :face all-the-icons-dsilver)

    ("\\.gz$"           all-the-icons-octicon "file-binary"             :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("\\.zip$"          all-the-icons-octicon "file-zip"                :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("\\.7z$"           all-the-icons-octicon "file-zip"                :v-adjust 0.0 :face all-the-icons-lmaroon)

    ("\\.dat$"          all-the-icons-faicon "bar-chart"                :face all-the-icons-cyan :v-adjust 0.0 :height 0.65)
    ;; lock files
    ("~$"               all-the-icons-octicon "lock"                    :v-adjust 0.0 :face all-the-icons-maroon)

    ("\\.dmg$"          all-the-icons-octicon "tools"                   :v-adjust 0.0 :face all-the-icons-lsilver)
    ("\\.dll$"          all-the-icons-faicon "cogs"                     :face all-the-icons-silver)
    ("\\.DS_STORE$"     all-the-icons-faicon "cogs"                     :face all-the-icons-silver)

    ;; Source Codes
    ("\\.scpt$"         all-the-icons-fileicon "apple"                  :face all-the-icons-pink)
    ("\\.aup$"          all-the-icons-fileicon "audacity"               :face all-the-icons-yellow)

    ("\\.elm$"          all-the-icons-fileicon "elm"                    :height 0.75 :v-adjust -0.15 :face all-the-icons-blue)

    ("\\.erl$"          all-the-icons-alltheicon "erlang"               :face all-the-icons-pink :v-adjust -0.1 :height 0.85)
    ("\\.hrl$"          all-the-icons-alltheicon "erlang"               :face all-the-icons-pink :v-adjust -0.1 :height 0.85)

    ("\\.eex$"          all-the-icons-alltheicon "elixir"               :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("\\.leex$"         all-the-icons-alltheicon "elixir"               :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("\\.ex$"           all-the-icons-alltheicon "elixir"               :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("\\.exs$"          all-the-icons-alltheicon "elixir"               :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("^mix.lock$"       all-the-icons-material   "lock"                 :v-adjust -0.2 :face all-the-icons-yellow)

    ("\\.java$"         all-the-icons-alltheicon "java"                 :face all-the-icons-lpurple)

    ("\\.go$"           all-the-icons-alltheicon "go"                   :face all-the-icons-lblue)

    ("\\.mp3$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.wav$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.m4a$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.ogg$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.flac$"         all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.opus$"         all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.au$"           all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.aif[fc]?$"     all-the-icons-faicon "volume-up"                :face all-the-icons-dred)

    ("\\.jl$"           all-the-icons-fileicon "julia"                  :v-adjust -0.1 :face all-the-icons-purple)
    ("\\.matlab$"       all-the-icons-fileicon "matlab"                 :face all-the-icons-orange)

    ("\\.nix$"          all-the-icons-fileicon "nix"                    :face all-the-icons-blue)

    ("\\.p[ml]$"        all-the-icons-alltheicon "perl"                 :face all-the-icons-lblue)
    ("\\.pl6$"          all-the-icons-fileicon "perl6"                  :face all-the-icons-lblue)
    ("\\.pm6$"          all-the-icons-fileicon "perl6"                  :face all-the-icons-lblue)
    ("\\.pod$"          all-the-icons-alltheicon "perldocs"             :height 1.2  :face all-the-icons-lgreen)

    ("\\.php$"          all-the-icons-fileicon "php"                    :height 0.85 :v-adjust -0.2 :face all-the-icons-dmaroon)
    ("\\.pony$"         all-the-icons-fileicon "pony"                   :face all-the-icons-maroon)
    ("\\.ps1$"          all-the-icons-fileicon "powershell"             :face all-the-icons-blue)
    ("\\.prol?o?g?$"    all-the-icons-alltheicon "prolog"               :height 1.1  :face all-the-icons-lmaroon)
    ("\\.py$"           all-the-icons-alltheicon "python"               :face all-the-icons-blue)
    ("\\.ipynb$"        all-the-icons-fileicon "jupyter"                :height 1.0  :face all-the-icons-dorange)

    ("\\.rkt$"          all-the-icons-fileicon "racket"                 :height 1.2 :face all-the-icons-red)
    ("^Gemfile\\(\\.lock\\)?$" all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    ("\\.gem$"          all-the-icons-alltheicon "ruby-alt"             :face all-the-icons-red)
    ("_?test\\.rb$"        all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-red)
    ("_?test_helper\\.rb$" all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-dred)
    ("_?spec\\.rb$"        all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-red)
    ("_?spec_helper\\.rb$" all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-dred)
    ("\\.rb$"           all-the-icons-octicon "ruby"                    :v-adjust 0.0 :face all-the-icons-red)
    ("\\.rs$"           all-the-icons-alltheicon "rust"                 :height 1.2  :face all-the-icons-maroon)
    ("\\.rlib$"         all-the-icons-alltheicon "rust"                 :height 1.2  :face all-the-icons-dmaroon)
    ("\\.r[ds]?x?$"     all-the-icons-fileicon "R"                      :face all-the-icons-lblue)

    ("\\.sbt$"          all-the-icons-fileicon   "sbt"                  :face all-the-icons-lred)
    ("\\.scala$"        all-the-icons-alltheicon "scala"                :face all-the-icons-lred)
    ("\\.scm$"          all-the-icons-fileicon   "scheme"               :height 1.2 :face all-the-icons-red)
    ("\\.swift$"        all-the-icons-alltheicon "swift"                :face all-the-icons-lblue)

    ("-?spec\\.ts$"     all-the-icons-fileicon "test-typescript"        :face all-the-icons-blue)
    ("-?test\\.ts$"     all-the-icons-fileicon "test-typescript"        :face all-the-icons-blue)
    ("-?spec\\.js$"     all-the-icons-fileicon "test-js"                :face all-the-icons-lpurple)
    ("-?test\\.js$"     all-the-icons-fileicon "test-js"                :face all-the-icons-lpurple)
    ("-?spec\\.jsx$"    all-the-icons-fileicon "test-react"             :face all-the-icons-dblue)
    ("-?test\\.jsx$"    all-the-icons-fileicon "test-react"             :face all-the-icons-dblue)

    ("-?spec\\."        all-the-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen)
    ("-?test\\."        all-the-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen)

    ("\\.tf\\(vars\\|state\\)?$" all-the-icons-fileicon "terraform"     :height 1.0 :face all-the-icons-purple-alt)

    ("\\.asm$"          all-the-icons-fileicon "assembly"               :height 1.0 :face all-the-icons-blue)

    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("\\.v$"             all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("\\.vams$"          all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("\\.sv$"            all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("\\.sva$"           all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("\\.svh$"           all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("\\.svams$"         all-the-icons-fileicon "verilog"               :height 1.0 :v-adjust -0.2 :face all-the-icons-red)

    ;; VHDL(-AMS)
    ("\\.vhd$"           all-the-icons-fileicon "vhdl"                  :face all-the-icons-blue)
    ("\\.vhdl$"          all-the-icons-fileicon "vhdl"                  :face all-the-icons-blue)
    ("\\.vhms$"          all-the-icons-fileicon "vhdl"                  :face all-the-icons-blue)

    ;; Cabal
    ("\\.cabal$"         all-the-icons-fileicon "cabal"                 :face all-the-icons-lblue)

    ;; Kotlin
    ("\\.kt$"            all-the-icons-fileicon "kotlin"                :face all-the-icons-orange)
    ("\\.kts$"           all-the-icons-fileicon "kotlin"                :face all-the-icons-orange)

    ;; Nimrod
    ("\\.nim$"           all-the-icons-fileicon "nimrod"                :face all-the-icons-yellow)
    ("\\.nims$"          all-the-icons-fileicon "nimrod"                :face all-the-icons-yellow)

    ;; SQL
    ("\\.sql$"           all-the-icons-octicon "database"               :face all-the-icons-silver)

    ;; Styles
    ("\\.styles$"        all-the-icons-material "style"                 :face all-the-icons-red)

    ;; Lua
    ("\\.lua$"           all-the-icons-fileicon "lua"                   :face all-the-icons-dblue)

    ;; ASCII doc
    ("\\.adoc$"          all-the-icons-fileicon "asciidoc"              :face all-the-icons-lblue)
    ("\\.asciidoc$"      all-the-icons-fileicon "asciidoc"              :face all-the-icons-lblue)

    ;; Puppet
    ("\\.pp$"            all-the-icons-fileicon "puppet"                :face all-the-icons-yellow)

    ;; Jinja
    ("\\.j2$"            all-the-icons-fileicon "jinja"                 :face all-the-icons-silver)
    ("\\.jinja2$"        all-the-icons-fileicon "jinja"                 :face all-the-icons-silver)

    ;; Docker
    ("\\.dockerfile$"    all-the-icons-fileicon "dockerfile"            :face all-the-icons-cyan)

    ;; Vagrant
    ("\\.vagrantfile$"   all-the-icons-fileicon "vagrant"               :face all-the-icons-blue)

    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("\\.c$"            all-the-icons-alltheicon "c-line"               :face all-the-icons-blue)
    ("\\.h$"            all-the-icons-alltheicon "c-line"               :face all-the-icons-purple)
    ("\\.m$"            all-the-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)
    ("\\.mm$"           all-the-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)

    ("\\.c\\(c\\|pp\\|xx\\)$"   all-the-icons-alltheicon "cplusplus-line"       :v-adjust -0.2 :face all-the-icons-blue)
    ("\\.h\\(h\\|pp\\|xx\\)$"   all-the-icons-alltheicon "cplusplus-line"       :v-adjust -0.2 :face all-the-icons-purple)

    ("\\.csx?$"         all-the-icons-alltheicon "csharp-line"          :face all-the-icons-dblue)

    ("\\.cljc?$"        all-the-icons-alltheicon "clojure-line"         :height 0.9 :v-adjust -0.1 :face all-the-icons-lgreen :v-adjust 0.0)
    ("\\.cljs$"         all-the-icons-fileicon "cljs"                   :height 0.9 :v-adjust -0.1 :face all-the-icons-dblue :v-adjust 0.0)

    ("\\.coffee$"       all-the-icons-alltheicon "coffeescript"         :height 0.85 :v-adjust -0.1 :face all-the-icons-blue-alt)
    ("\\.iced$"         all-the-icons-alltheicon "coffeescript"         :height 0.85 :v-adjust -0.1 :face all-the-icons-blue-alt)

    ("\\.dart$"         all-the-icons-fileicon "dart"                   :height 1.0 :face all-the-icons-blue :v-adjust 0.0)

    ;; Git
    ("^MERGE_"          all-the-icons-octicon "git-merge"               :v-adjust 0.0 :face all-the-icons-red)
    ("^COMMIT_EDITMSG"  all-the-icons-octicon "git-commit"              :v-adjust 0.0 :face all-the-icons-red)

    ;; Lisps
    ("\\.cl$"           all-the-icons-fileicon "clisp"                  :face all-the-icons-lorange)
    ("\\.l\\(isp\\)?$"  all-the-icons-fileicon "lisp"                   :face all-the-icons-orange)
    ("\\.el$"           all-the-icons-fileicon "elisp"                  :height 0.65 :v-adjust -0.2 :face all-the-icons-purple-alt)

    ;; Stylesheeting
    ("\\.css$"          all-the-icons-alltheicon "css3"                 :face all-the-icons-lblue)
    ("\\.scss$"         all-the-icons-alltheicon "sass"                 :face all-the-icons-pink)
    ("\\.sass$"         all-the-icons-alltheicon "sass"                 :face all-the-icons-dpink)
    ("\\.less$"         all-the-icons-alltheicon "less"                 :height 0.8  :face all-the-icons-dyellow)
    ("\\.postcss$"      all-the-icons-fileicon "postcss"                :face all-the-icons-dred)
    ("\\.sss$"          all-the-icons-fileicon "postcss"                :face all-the-icons-dred)
    ("\\.styl$"         all-the-icons-alltheicon "stylus"               :face all-the-icons-lgreen)
    ("stylelint"        all-the-icons-fileicon "stylelint"              :face all-the-icons-lyellow)
    ("\\.csv$"          all-the-icons-octicon "graph"                   :v-adjust 0.0 :face all-the-icons-dblue)

    ("\\.hs$"           all-the-icons-alltheicon "haskell"              :face all-the-icons-orange)
    ("\\.chs$"          all-the-icons-alltheicon "haskell"              :face all-the-icons-orange)
    ("\\.lhs$"          all-the-icons-alltheicon "haskell"              :face all-the-icons-orange)
    ("\\.hsc$"          all-the-icons-alltheicon "haskell"              :face all-the-icons-orange)

    ;; Web modes
    ("\\.inky-haml$"    all-the-icons-fileicon "haml"                   :height 0.80 :v-adjust -0.2 :face all-the-icons-lyellow)
    ("\\.haml$"         all-the-icons-fileicon "haml"                   :height 0.80 :v-adjust -0.2 :face all-the-icons-lyellow)
    ("\\.html?$"        all-the-icons-alltheicon "html5"                :face all-the-icons-orange)
    ("\\.inky-erb?$"    all-the-icons-alltheicon "html5"                :face all-the-icons-lred)
    ("\\.erb$"          all-the-icons-alltheicon "html5"                :face all-the-icons-red-alt)
    ("\\.hbs$"          all-the-icons-fileicon "moustache"              :face all-the-icons-orange)
    ("\\.inky-slim$"    all-the-icons-octicon "dashboard"               :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.slim$"         all-the-icons-octicon "dashboard"               :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.jade$"         all-the-icons-fileicon "jade"                   :face all-the-icons-red)
    ("\\.pug$"          all-the-icons-fileicon "pug-alt"                :face all-the-icons-red)

    ;; JavaScript
    ("^gulpfile"        all-the-icons-alltheicon "gulp"                 :face all-the-icons-lred)
    ("^gulpfile.js"     all-the-icons-alltheicon "gulp"                 :face all-the-icons-lred)
    ("^gruntfile"       all-the-icons-alltheicon "grunt"                :face all-the-icons-dyellow)
    ("^gruntfile.js"    all-the-icons-alltheicon "grunt"                :face all-the-icons-dyellow)
    ("^webpack"         all-the-icons-fileicon "webpack"                :face all-the-icons-lblue)

    ("\\.d3\\.?js"      all-the-icons-alltheicon "d3"                   :height 0.8  :face all-the-icons-lgreen)

    ("\\.re$"            all-the-icons-fileicon "reason"                :face all-the-icons-cyan)
    ("\\.rei$"           all-the-icons-fileicon "reason"                :face all-the-icons-cyan)
    ("\\.ml$"            all-the-icons-fileicon "ocaml"                 :v-adjust -0.1 :face all-the-icons-dyellow)
    ("\\.mli$"           all-the-icons-fileicon "ocaml"                 :v-adjust -0.1 :face all-the-icons-dyellow)

    ("\\.react"         all-the-icons-alltheicon "react"                :height 1.1  :face all-the-icons-lblue)
    ("\\.d\\.ts$"       all-the-icons-fileicon "typescript"             :face all-the-icons-cyan-alt)
    ("\\.ts$"           all-the-icons-fileicon "typescript"             :face all-the-icons-blue-alt)
    ("\\.tsx$"          all-the-icons-fileicon "typescript"             :face all-the-icons-blue)
    ("\\.js$"           all-the-icons-alltheicon "javascript"           :face all-the-icons-yellow)
    ("\\.es[0-9]$"      all-the-icons-alltheicon "javascript"           :face all-the-icons-yellow)
    ("\\.jsx$"          all-the-icons-fileicon "jsx-2"                  :height 0.8 :v-adjust -0.1 :face all-the-icons-dblue)
    ("\\.njs$"          all-the-icons-alltheicon "nodejs"               :height 1.2  :face all-the-icons-lgreen)
    ("\\.vue$"          all-the-icons-fileicon "vue"                    :face all-the-icons-lgreen)

    ;; F#
    ("\\.fs[ix]?$"      all-the-icons-fileicon "fsharp"                 :face all-the-icons-blue-alt)

    ;; File Types
    ("\\.ico$"          all-the-icons-material "image"              :v-adjust -0.25 :height 0.85 :face all-the-icons-blue)
    ("\\.png$"          all-the-icons-material "image"              :v-adjust -0.25 :height 0.85 :face all-the-icons-cyan-alt)
    ("\\.gif$"          all-the-icons-material "image"              :v-adjust -0.25 :height 0.85 :face all-the-icons-green)
    ("\\.jpe?g$"        all-the-icons-material "image"              :v-adjust -0.25 :height 0.85 :face all-the-icons-dblue)
    ("\\.svg$"          all-the-icons-alltheicon "svg"              :height 0.9  :face all-the-icons-lgreen)

    ;; Video
    ("\\.mov$"          all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.mp4$"          all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.mpg$"          all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.mpeg$"         all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.ogv$"          all-the-icons-faicon "film"                     :face all-the-icons-dblue)
    ("\\.mkv$"          all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.webm$"         all-the-icons-faicon "film"                     :face all-the-icons-blue)

    ;; Fonts
    ("\\.ttf$"          all-the-icons-fileicon "font"                   :v-adjust 0.0 :face all-the-icons-dcyan)
    ("\\.woff2?$"       all-the-icons-fileicon "font"                   :v-adjust 0.0 :face all-the-icons-cyan)

    ;; Doc
    ("\\.pdf$"          all-the-icons-octicon "file-pdf"                :v-adjust 0.0 :face all-the-icons-dred)
    ("\\.te?xt$"        all-the-icons-material "subject"               :v-adjust 0.0 :face all-the-icons-lcyan)
    ("\\.doc[xm]?$"     all-the-icons-fileicon "word"                   :face all-the-icons-blue)
    ("\\.texi?$"        all-the-icons-fileicon "tex"                    :face all-the-icons-lred)
    ("\\.md$"           all-the-icons-octicon "markdown"                :v-adjust -0.05 :height 0.8 :face all-the-icons-lblue)
    ("\\.bib$"          all-the-icons-fileicon "bib"                    :face all-the-icons-maroon)
    ("\\.org$"          all-the-icons-fileicon "org"                    :face all-the-icons-lgreen)

    ("\\.pp[st]$"       all-the-icons-fileicon "powerpoint"             :face all-the-icons-orange)
    ("\\.pp[st]x$"      all-the-icons-fileicon "powerpoint"             :face all-the-icons-red)
    ("\\.knt$"          all-the-icons-fileicon "powerpoint"             :face all-the-icons-cyan)

    ("bookmark"         all-the-icons-octicon "bookmark"                :height 1.1 :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.cache$"        all-the-icons-octicon "database"                :height 1.0 :v-adjust 0.0 :face all-the-icons-green)

    ("^\\*scratch\\*$"  all-the-icons-faicon "sticky-note"              :face all-the-icons-lyellow)
    ("^\\*scratch.*"    all-the-icons-faicon "sticky-note"              :face all-the-icons-yellow)
    ("^\\*new-tab\\*$"  all-the-icons-material "star"                     :face all-the-icons-cyan)

    ("^\\."             all-the-icons-octicon "gear"                    :v-adjust 0.0)
    (".?"               all-the-icons-faicon "file-o"                   :v-adjust 0.0 :face all-the-icons-dsilver)))

(setq all-the-icons-dir-icon-alist
  '(
    ("trash"            all-the-icons-faicon "trash-o"          :height 1.2 :v-adjust -0.1)
    ("dropbox"          all-the-icons-faicon "dropbox"          :height 1.0 :v-adjust -0.1)
    ("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 1.0 :v-adjust -0.1)
    ("^atom$"           all-the-icons-alltheicon "atom"         :height 1.2 :v-adjust -0.1)
    ("documents"        all-the-icons-faicon "book"             :height 1.0 :v-adjust -0.1)
    ("download"         all-the-icons-faicon "cloud-download"   :height 0.9 :v-adjust -0.1)
    ("desktop"          all-the-icons-octicon "device-desktop"  :height 1.0 :v-adjust -0.1)
    ("pictures"         all-the-icons-faicon "picture-o"        :height 0.9 :v-adjust -0.2)
    ("photos"           all-the-icons-faicon "camera-retro"     :height 1.0 :v-adjust -0.1)
    ("music"            all-the-icons-faicon "music"            :height 1.0 :v-adjust -0.1)
    ("movies"           all-the-icons-faicon "film"             :height 0.9 :v-adjust -0.1)
    ("code"             all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("workspace"        all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("test"             all-the-icons-fileicon "test-dir"       :height 0.9)
    ("\\.git"           all-the-icons-alltheicon "git"          :height 1.0)
    (".?"               all-the-icons-octicon "file-directory"  :height 1.0 :v-adjust -0.1)
    ))

(setq all-the-icons-weather-icon-alist
  '(
    ("tornado"               all-the-icons-wicon "tornado")
    ("hurricane"             all-the-icons-wicon "hurricane")
    ("thunderstorms"         all-the-icons-wicon "thunderstorm")
    ("sunny"                 all-the-icons-wicon "day-sunny")
    ("rain.*snow"            all-the-icons-wicon "rain-mix")
    ("rain.*hail"            all-the-icons-wicon "rain-mix")
    ("sleet"                 all-the-icons-wicon "sleet")
    ("hail"                  all-the-icons-wicon "hail")
    ("drizzle"               all-the-icons-wicon "sprinkle")
    ("rain"                  all-the-icons-wicon "showers" :height 1.1 :v-adjust 0.0)
    ("showers"               all-the-icons-wicon "showers")
    ("blowing.*snow"         all-the-icons-wicon "snow-wind")
    ("snow"                  all-the-icons-wicon "snow")
    ("dust"                  all-the-icons-wicon "dust")
    ("fog"                   all-the-icons-wicon "fog")
    ("haze"                  all-the-icons-wicon "day-haze")
    ("smoky"                 all-the-icons-wicon "smoke")
    ("blustery"              all-the-icons-wicon "cloudy-windy")
    ("windy"                 all-the-icons-wicon "cloudy-gusts")
    ("cold"                  all-the-icons-wicon "snowflake-cold")
    ("partly.*cloudy.*night" all-the-icons-wicon "night-alt-partly-cloudy")
    ("partly.*cloudy"        all-the-icons-wicon "day-cloudy-high")
    ("cloudy.*night"         all-the-icons-wicon "night-alt-cloudy")
    ("cxloudy.*day"          all-the-icons-wicon "day-cloudy")
    ("cloudy"                all-the-icons-wicon "cloudy")
    ("clear.*night"          all-the-icons-wicon "night-clear")
    ("fair.*night"           all-the-icons-wicon "stars")
    ("fair.*day"             all-the-icons-wicon "horizon")
    ("hot"                   all-the-icons-wicon "hot")
    ("not.*available"        all-the-icons-wicon "na")
    ))

(setq all-the-icons-mode-icon-alist
  '(
    (emacs-lisp-mode           all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.1 :face all-the-icons-purple-alt)
    (circe-server-mode         all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (circe-channel-mode        all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (erc-mode                  all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (inferior-emacs-lisp-mode  all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.1 :face all-the-icons-lblue)
    (dired-mode                all-the-icons-octicon "file-directory"      :v-adjust 0.0)
    (lisp-interaction-mode     all-the-icons-fileicon "lisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (sly-mrepl-mode            all-the-icons-fileicon "clisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (slime-repl-mode           all-the-icons-fileicon "clisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (org-mode                  all-the-icons-fileicon "org"                :v-adjust 0.0 :face all-the-icons-lgreen)
    (typescript-mode           all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
    (js-mode                   all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js-jsx-mode               all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js2-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js3-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (rjsx-mode                 all-the-icons-fileicon "jsx-2"              :v-adjust -0.1 :face all-the-icons-cyan-alt)
    (term-mode                 all-the-icons-octicon "terminal"            :v-adjust 0.2)
    (vterm-mode                all-the-icons-octicon "terminal"            :v-adjust 0.2)
    (eshell-mode               all-the-icons-octicon "terminal"            :v-adjust 0.0 :face all-the-icons-purple)
    (magit-refs-mode           all-the-icons-octicon "git-branch"          :v-adjust 0.0 :face all-the-icons-red)
    (magit-process-mode        all-the-icons-octicon "mark-github"         :v-adjust 0.0)
    (magit-diff-mode           all-the-icons-octicon "git-compare"         :v-adjust 0.0 :face all-the-icons-lblue)
    (ediff-mode                all-the-icons-octicon "git-compare"         :v-adjust 0.0 :Face all-the-icons-red)
    (comint-mode               all-the-icons-faicon "terminal"             :v-adjust 0.0 :face all-the-icons-lblue)
    (eww-mode                  all-the-icons-faicon "firefox"              :v-adjust -0.1 :face all-the-icons-red)
    (org-agenda-mode           all-the-icons-octicon "checklist"           :v-adjust 0.0 :face all-the-icons-lgreen)
    (cfw:calendar-mode         all-the-icons-octicon "calendar"            :v-adjust 0.0)
    (ibuffer-mode              all-the-icons-faicon "files-o"              :v-adjust 0.0 :face all-the-icons-dsilver)
    (messages-buffer-mode      all-the-icons-faicon "stack-overflow"       :v-adjust -0.1)
    (help-mode                 all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
    (benchmark-init/tree-mode  all-the-icons-octicon "dashboard"           :v-adjust 0.0)
    (jenkins-mode              all-the-icons-fileicon "jenkins"            :face all-the-icons-blue)
    (magit-popup-mode          all-the-icons-alltheicon "git"              :face all-the-icons-red)
    (magit-status-mode         all-the-icons-alltheicon "git"              :face all-the-icons-lred)
    (magit-log-mode            all-the-icons-alltheicon "git"              :face all-the-icons-green)
    (mu4e-compose-mode         all-the-icons-octicon "pencil"              :v-adjust 0.0)
    (mu4e-headers-mode         all-the-icons-octicon "mail"                :v-adjust 0.0)
    (mu4e-main-mode            all-the-icons-octicon "mail"                :v-adjust 0.0)
    (mu4e-view-mode            all-the-icons-octicon "mail-read"           :v-adjust 0.0)
    (paradox-menu-mode         all-the-icons-faicon "archive"              :height 1.0 :v-adjust 0.0 :face all-the-icons-silver)
    (Custom-mode               all-the-icons-octicon "settings")

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode             all-the-icons--web-mode-icon)

    (fundamental-mode                   all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-dsilver)
    (special-mode                       all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-yellow)
    (text-mode                          all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
    (enh-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (ruby-mode                          all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (inf-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (projectile-rails-compilation-mode  all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rspec-compilation-mode             all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rake-compilation-mode              all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (sh-mode                            all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    (shell-mode                         all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    (fish-mode                          all-the-icons-alltheicon "terminal"       :face all-the-icons-lpink)
    (nginx-mode                         all-the-icons-fileicon "nginx"            :height 0.9  :face all-the-icons-dgreen)
    (apache-mode                        all-the-icons-alltheicon "apache"         :height 0.9  :face all-the-icons-dgreen)
    (makefile-mode                      all-the-icons-fileicon "gnu"              :face all-the-icons-dorange)
    (dockerfile-mode                    all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
    (docker-compose-mode                all-the-icons-fileicon "dockerfile"       :face all-the-icons-lblue)
    (nxml-mode                          all-the-icons-faicon "file-code-o"        :height 0.95 :face all-the-icons-lorange)
    (json-mode                          all-the-icons-octicon "settings"          :face all-the-icons-dpurple)
    (yaml-mode                          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    (elisp-byte-code-mode               all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-dsilver)
    (archive-mode                       all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
    (elm-mode                           all-the-icons-fileicon "elm"              :face all-the-icons-blue)
    (erlang-mode                        all-the-icons-alltheicon "erlang"         :face all-the-icons-red :v-adjust -0.1 :height 0.9)
    (elixir-mode                        all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    (java-mode                          all-the-icons-alltheicon "java"           :height 1.0  :face all-the-icons-lpurple)
    (go-mode                            all-the-icons-fileicon "go"               :height 1.0  :face all-the-icons-blue)
    (matlab-mode                        all-the-icons-fileicon "matlab"           :face all-the-icons-orange)
    (perl-mode                          all-the-icons-alltheicon "perl"           :face all-the-icons-lblue)
    (cperl-mode                         all-the-icons-alltheicon "perl"           :face all-the-icons-lblue)
    (php-mode                           all-the-icons-fileicon "php"              :face all-the-icons-dmaroon)
    (prolog-mode                        all-the-icons-alltheicon "prolog"         :height 1.1  :face all-the-icons-lmaroon)
    (python-mode                        all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-blue)
    (inferior-python-mode               all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-blue)
    (racket-mode                        all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-red)
    (rust-mode                          all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    (scala-mode                         all-the-icons-alltheicon "scala"          :face all-the-icons-red)
    (scheme-mode                        all-the-icons-fileicon   "scheme"         :height 1.2 :face all-the-icons-red)
    (swift-mode                         all-the-icons-alltheicon "swift"          :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
    (c-mode                             all-the-icons-alltheicon "c-line"         :face all-the-icons-blue)
    (c++-mode                           all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    (csharp-mode                        all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    (clojure-mode                       all-the-icons-alltheicon "clojure"        :height 1.0  :face all-the-icons-blue)
    (cider-repl-mode                    all-the-icons-alltheicon "clojure"        :height 1.0  :face all-the-icons-green)
    (clojurescript-mode                 all-the-icons-fileicon "cljs"             :height 1.0  :face all-the-icons-dblue)
    (coffee-mode                        all-the-icons-alltheicon "coffeescript"   :height 1.0  :face all-the-icons-maroon)
    (lisp-mode                          all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
    (css-mode                           all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
    (scss-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-pink)
    (sass-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-dpink)
    (less-css-mode                      all-the-icons-alltheicon "less"           :height 0.8  :face all-the-icons-dyellow)
    (stylus-mode                        all-the-icons-alltheicon "stylus"         :face all-the-icons-lgreen)
    (csv-mode                           all-the-icons-octicon "graph"             :v-adjust 0.0 :face all-the-icons-dblue)
    (haskell-mode                       all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-orange)
    (haskell-c2hs-mode                  all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-orange)
    (literate-haskell-mode              all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-orange)
    (haml-mode                          all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
    (html-mode                          all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
    (rhtml-mode                         all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
    (mustache-mode                      all-the-icons-fileicon "moustache"        :face all-the-icons-green)
    (slim-mode                          all-the-icons-octicon "dashboard"         :v-adjust 0.0 :face all-the-icons-yellow)
    (jade-mode                          all-the-icons-fileicon "jade"             :face all-the-icons-red)
    (pug-mode                           all-the-icons-fileicon "pug"              :face all-the-icons-red)
    (react-mode                         all-the-icons-alltheicon "react"          :height 1.1  :face all-the-icons-lblue)
    (image-mode                         all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-blue)
    (texinfo-mode                       all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (markdown-mode                      all-the-icons-octicon "markdown"          :v-adjust 0.0 :face all-the-icons-lblue)
    (bibtex-mode                        all-the-icons-fileicon "bib"              :face all-the-icons-maroon)
    (org-mode                           all-the-icons-fileicon "org"              :face all-the-icons-lgreen)
    (compilation-mode                   all-the-icons-faicon "cogs"               :v-adjust 0.0 :height 1.0)
    (objc-mode                          all-the-icons-faicon "apple"              :v-adjust 0.0 :height 1.0)
    (tuareg-mode                        all-the-icons-fileicon "ocaml"            :v-adjust 0.0 :height 1.0)
    (purescript-mode                    all-the-icons-fileicon "purescript"       :v-adjust 0.0 :height 1.0)
    (verilog-mode                       all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    (vhdl-mode                          all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
    (haskell-cabal-mode                 all-the-icons-fileicon "cabal"            :face all-the-icons-lblue)
    (kotlin-mode                        all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
    (nim-mode                           all-the-icons-fileicon "nimrod"           :face all-the-icons-yellow)
    (sql-mode                           all-the-icons-octicon  "database"         :face all-the-icons-silver)
    (lua-mode                           all-the-icons-fileicon "lua"              :face all-the-icons-dblue)
    (adoc-mode                          all-the-icons-fileicon "asciidoc"         :face all-the-icons-lblue)
    (puppet-mode                        all-the-icons-fileicon "puppet"           :face all-the-icons-yellow)
    (jinja2-mode                        all-the-icons-fileicon "jinja"            :face all-the-icons-silver)
    (powershell-mode                    all-the-icons-fileicon "powershell"       :face all-the-icons-blue)
    (tex-mode                           all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (latex-mode                         all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (dart-mode                          all-the-icons-fileicon "dart"             :height 1.0  :face all-the-icons-blue)
    (fsharp-mode                        all-the-icons-fileicon "fsharp"           :height 1.0  :face all-the-icons-blue)
    (asm-mode                           all-the-icons-fileicon "assembly"         :height 1.0  :face all-the-icons-blue)
    (nasm-mode                          all-the-icons-fileicon "assembly"         :height 1.0  :face all-the-icons-blue)
    ))

(setq all-the-icons-url-alist
  '(
    ;; Social media and communities
    ("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us" all-the-icons-faicon "delicious")
    ("^\\(https?://\\)?\\(www\\.\\)?behance\\.net" all-the-icons-faicon "behance")
    ("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com" all-the-icons-faicon "dribbble")
    ("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com" all-the-icons-faicon "facebook-official")
    ("^\\(https?://\\)?\\(www\\.\\)?glide\\.me" all-the-icons-faicon "glide-g")
    ("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com" all-the-icons-faicon "google-plus")
    ("linkedin\\.com" all-the-icons-faicon "linkedin")
    ("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru" all-the-icons-faicon "odnoklassniki")
    ("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com" all-the-icons-faicon "reddit-alien")
    ("^\\(https?://\\)?\\(www\\.\\)?slack\\.com" all-the-icons-faicon "slack")
    ("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com" all-the-icons-faicon "snapchat-ghost")
    ("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com" all-the-icons-faicon "weibo")
    ("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com" all-the-icons-faicon "twitter")
    ;; Blogging
    ("joomla\\.org" all-the-icons-faicon "joomla")
    ("^\\(https?://\\)?\\(www\\.\\)?medium\\.com" all-the-icons-faicon "medium")
    ("tumblr\\.com" all-the-icons-faicon "tumblr")
    ("^wordpress\\.com" all-the-icons-faicon "wordpress")
    ;; Programming
    ("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org" all-the-icons-octicon "bitbucket")
    ("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io" all-the-icons-faicon "codepen")
    ("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com" all-the-icons-faicon "codiepie")
    ("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com" all-the-icons-octicon "gist")
    ("^\\(https?://\\)?\\(www\\.\\)?github\\.com" all-the-icons-octicon "mark-github")
    ("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com" all-the-icons-faicon "gitlab")
    ("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" all-the-icons-faicon "hacker-news")
    ("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net" all-the-icons-faicon "jsfiddle")
    ("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com" all-the-icons-faicon "maxcdn")
    ("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com" all-the-icons-faicon "stack-overflow")
    ;; Video
    ("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv" all-the-icons-faicon "twitch")
    ("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com" all-the-icons-faicon "vimeo")
    ("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com" all-the-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be" all-the-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?vine\\.co" all-the-icons-faicon "vine")
    ;; Sound
    ("^\\(https?://\\)?\\(www\\.\\)?last\\.fm" all-the-icons-faicon "lastfm")
    ("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com" all-the-icons-faicon "mixcloud")
    ("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com" all-the-icons-faicon "soundcloud")
    ("spotify\\.com" all-the-icons-faicon "spotify")
    ;; Shopping
    ("^\\(https?://\\)?\\(www\\.\\)?amazon\\." all-the-icons-faicon "amazon")
    ("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com" all-the-icons-faicon "opencart")
    ("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com" all-the-icons-faicon "paypal")
    ("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com" all-the-icons-faicon "shitsinbulk")
    ;; Images
    ("^\\(https?://\\)?\\(www\\.\\)?500px\\.com" all-the-icons-faicon "500px")
    ("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com" all-the-icons-faicon "deviantart")
    ("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com" all-the-icons-faicon "flickr")
    ("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com" all-the-icons-faicon "instagram")
    ("^\\(https?://\\)?\\(www\\.\\)?pinterest\\." all-the-icons-faicon "pinterest")
    ;; Information and books
    ("^\\(https?://\\)?\\(www\\.\\)?digg\\.com" all-the-icons-faicon "digg")
    ("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com" all-the-icons-faicon "foursquare")
    ("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com" all-the-icons-faicon "get-pocket")
    ("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com" all-the-icons-faicon "scribd")
    ("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net" all-the-icons-faicon "slideshare")
    ("stackexchange\\.com" all-the-icons-faicon "stack-exchange")
    ("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com" all-the-icons-faicon "stumbleupon")
    ("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\." all-the-icons-faicon "tripadvisor")
    ("^\\(https?://\\)?\\(www\\.\\)?yelp\\." all-the-icons-faicon "yelp")

    ("wikipedia\\.org" all-the-icons-faicon "wikipedia-w")
    ;; Various companies and tools
    ("^\\(https?://\\)?\\(www\\.\\)?angel\\.co" all-the-icons-faicon "angellist")
    ("^\\(https?://\\)?\\(www\\.\\)?apple\\.com" all-the-icons-faicon "apple")
    ("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com" all-the-icons-faicon "buysellads")
    ("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com" all-the-icons-faicon "connectdevelop")
    ("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com" all-the-icons-faicon "dashcube")
    ("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com" all-the-icons-faicon "dropbox")
    ("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com" all-the-icons-faicon "envira")
    ("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com" all-the-icons-faicon "fort-awesome")
    ("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com" all-the-icons-faicon "forumbee")
    ("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com" all-the-icons-faicon "gratipay")
    ("^\\(https?://\\)?\\(www\\.\\)?modx\\.com" all-the-icons-faicon "modx")
    ("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com" all-the-icons-faicon "pagelines")
    ("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com" all-the-icons-faicon "product-hunt")
    ("sellsy\\.com" all-the-icons-faicon "sellsy")
    ("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com" all-the-icons-faicon "simplybuilt")
    ("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com" all-the-icons-faicon "skyatlas")
    ("^\\(https?://\\)?\\(www\\.\\)?skype\\.com" all-the-icons-faicon "skype")
    ("steampowered\\.com" all-the-icons-faicon "steam")
    ("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com" all-the-icons-faicon "themeisle")
    ("^\\(https?://\\)?\\(www\\.\\)?trello\\.com" all-the-icons-faicon "trello")
    ("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com" all-the-icons-faicon "whatsapp")
    ("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com" all-the-icons-faicon "y-combinator")
    ("yahoo\\.com" all-the-icons-faicon "yahoo")
    ("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com" all-the-icons-faicon "yoast")
    ;; Catch all
    ("android" all-the-icons-faicon "android")
    ("creativecommons" all-the-icons-faicon "creative-commons")
    ("forums?" all-the-icons-octicon "comment-discussion")
    ("\\.pdf$" all-the-icons-octicon "file-pdf" :v-adjust 0.0 :face all-the-icons-dred)
    ("google" all-the-icons-faicon "google")
    ("\\.rss" all-the-icons-faicon "rss")
    ))
