
(require 'asdf)

(asdf:load-system "tungsten")

(tungsten.check:test-system-and-exit "tungsten")
