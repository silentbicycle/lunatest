-- @module suite_hooks
local suite_hooks = {}

local lunatest = package.loaded.lunatest
local assert_true = lunatest.assert_true

function suite_hooks.suite_setup()
   print "\n\n-- running suite setup hook"
end

function suite_hooks.suite_teardown()
   print "\n\n-- running suite teardown hook"
end

function suite_hooks.test_ok()
   assert_true(true)
end

return suite_hooks
