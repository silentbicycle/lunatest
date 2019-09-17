pcall(require, "luacov")    --measure code coverage, if luacov is present
local lunatest = require "lunatest"
local assert_true, assert_false = lunatest.assert_true, lunatest.assert_false
local assert_diffvars = lunatest.assert_diffvars
local assert_boolean, assert_not_boolean = lunatest.assert_boolean, lunatest.assert_not_boolean
local assert_len, assert_not_len = lunatest.assert_len, lunatest.assert_not_len
local assert_match, assert_not_match = lunatest.assert_match, lunatest.assert_not_match
local assert_error = lunatest.assert_error
local assert_lt, assert_lte = lunatest.assert_lt, lunatest.assert_lte
local assert_gt, assert_gte = lunatest.assert_gt, lunatest.assert_gte
local assert_nil, assert_not_nil = lunatest.assert_nil, lunatest.assert_not_nil
local assert_equal, assert_not_equal = lunatest.assert_equal, lunatest.assert_not_equal
local assert_string, assert_not_string = lunatest.assert_string, lunatest.assert_not_string
local assert_metatable, assert_not_metatable = lunatest.assert_metatable, lunatest.assert_not_metatable
local assert_userdata, assert_not_userdata = lunatest.assert_userdata, lunatest.assert_not_userdata
local assert_thread, assert_not_thread = lunatest.assert_thread, lunatest.assert_not_thread
local assert_function, assert_not_function = lunatest.assert_function, lunatest.assert_not_function
local assert_table, assert_not_table = lunatest.assert_table, lunatest.assert_not_table
local assert_number, assert_not_number = lunatest.assert_number, lunatest.assert_not_number
local skip, fail = lunatest.skip, lunatest.fail

print '=============================='
print('To test just the random suite, add "-s random" to the command line')
print('To run just some tests, add "-t [pattern]"')
print '=============================='


lunatest.suite("suite-with-random-tests")
lunatest.suite("suite-hooks")
lunatest.suite("suite-hooks-fail")

function test_fail()
   -- the true here is so the test run as a whole still succeeds.
   fail("This one *should* fail.", true)
end

function test_assert()
   assert_true(true)
end

function test_skip()
   skip("(reason why this test was skipped)")
end

function test_assert_false()
   assert_false(false)
end

function test_assert_nil()
   assert_nil(nil)
end


function test_assert_not_nil()
   assert_not_nil("foo")
end

function test_assert_equal()
   assert_equal(4, 4)
end

function test_assert_equal_tolerance()
   assert_equal(4, 4.0001, 0.0001, "Should approximately match")
end

function test_assert_not_equal()
   assert_not_equal("perl", "quality")
end

function test_assert_gt()
   assert_gt(8, 400)
end

function test_assert_gte()
   assert_gte(8, 400)
   assert_gte(8, 8)
end

function test_assert_lt()
   assert_lt(8, -2)
end

function test_assert_lte()
   assert_lte(8, -2)
   assert_lte(8, 8)
end

function test_assert_len()
   assert_len(3, { "foo", "bar", "baz" })
end

function test_assert_not_len()
   assert_not_len(23, { "foo", "bar", "baz" })
end

function test_assert_match()
   assert_match("oo", "string with foo in it")
end

function test_assert_not_match()
   assert_not_match("abba zabba", "foo")
end

function test_assert_boolean()
   assert_boolean(true)
   assert_boolean(false)
end

function test_assert_not_boolean()
   assert_not_boolean("cheesecake")
end

function test_assert_number()
   assert_number(47)
   assert_number(0)
   assert_number(math.huge)
   assert_number(-math.huge)
end

function test_assert_not_number()
   assert_not_number(_G)
   assert_not_number("abc")
   assert_not_number({1, 2, 3})
   assert_not_number(false)
   assert_not_number(function () return 3 end)
end

function test_assert_string()
   assert_string("yarn")
   assert_string("")
end

function test_assert_not_string()
   assert_not_string(23)
   assert_not_string(true)
   assert_not_string(false)
   assert_not_string({"1", "2", "3"})
end

function test_assert_table()
   assert_table({})
   assert_table({"1", "2", "3"})
   assert_table({ foo=true, bar=true, baz=true })
end

function test_assert_not_table()
   assert_not_table(nil)
   assert_not_table(23)
   assert_not_table("lapdesk")
   assert_not_table(false)
   assert_not_table(function () return 3 end)
end

function test_assert_function()
   assert_function(function() return "*splat*" end)
   assert_function(string.format)
end

function test_assert_not_function()
   assert_not_function(nil)
   assert_not_function(23)
   assert_not_function("lapdesk")
   assert_not_function(false)
   assert_not_function(coroutine.create(function () return 3 end))
   assert_not_function({"1", "2", "3"})
   assert_not_function({ foo=true, bar=true, baz=true })
end

function test_assert_thread()
   assert_thread(coroutine.create(function () return 3 end))
end

function test_assert_not_thread()
   assert_not_thread(nil)
   assert_not_thread(23)
   assert_not_thread("lapdesk")
   assert_not_thread(false)
   assert_not_thread(function () return 3 end)
   assert_not_thread({"1", "2", "3"})
   assert_not_thread({ foo=true, bar=true, baz=true })
end

function test_assert_userdata()
   assert_userdata(io.open("test.lua", "r"))
end

function test_assert_not_userdata()
   assert_not_userdata(nil)
   assert_not_userdata(23)
   assert_not_userdata("lapdesk")
   assert_not_userdata(false)
   assert_not_userdata(function () return 3 end)
   assert_not_userdata({"1", "2", "3"})
   assert_not_userdata({ foo=true, bar=true, baz=true })
end

function test_assert_metatable()
   assert_metatable(getmetatable("any string"), "foo")
   local t = { __index=string }
   local val = setmetatable( { 1 }, t)
   assert_metatable(t, val)
end

function test_assert_not_metatable()
   assert_not_metatable(getmetatable("any string"), 23)
end

function test_assert_error()
   assert_error(function ()
                   error("*crash!*")
                end)
end

function test_assert_error_with_args()
   assert_error(function (a)
                   if a ~= nil then
                      error("*crash!*")
                   end
                end,
                "function should crash when argument 1 is set",
                "arg1")
end

-- This caused a crash when matching a string with invalid % escapes.
-- Thanks to Diab Jerius for the bugfix.
function test_failure_formatting()
   local inv_esc = "str with invalid escape %( in it"
   assert_match(inv_esc, inv_esc, "Should fail but not crash")
end

lunatest.run()
