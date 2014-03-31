-- @module suite-hooks-fail
local suite_hooks_fail = {}

local lunatest = package.loaded.lunatest
local assert_true = lunatest.assert_true

-- Either returning false or erroring out in suite_setup()
-- will prevent the suite from running.
function suite_hooks_fail.suite_setup()
   print "\n\n-- (about to fail and abort suite)"
   if true then return false end
   error("don't run this suite")
end

function suite_hooks_fail.test_never_run()
   assert_true(false, "this suite should never be run")
end

return suite_hooks_fail
