package = "lunatest"
version = "scm-0"
source = {
   url = "git://github.com/jdesgats/lunatest.git",
   branch = "master"
}
description = {
   summary = "xUnit-style + randomized unit testing framework",
   detailed = [[

Lunatest is an xUnit-style unit testing framework, with
additional support for randomized testing (a la QuickCheck).

It's upwardly compatible from lunit, except it does not change any
functions in the standard library (by using assert_true() instead
of assert()).
]],
   homepage = "http://github.com/silentbicycle/lunatest",
   license = "MIT/X11"
}
dependencies = {
   "lua >= 5.1"
}
build = {
   type = "builtin",
   modules = {
      lunatest = "lunatest.lua"
   }
}
