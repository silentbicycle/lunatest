#!/usr/local/bin/lua
--
-- Usage:
-- moonunit modulename [-v]


-- http://lua-users.org/wiki/LuaModuleFunctionCritiqued
local M = {}                    -- module

-- local helper funs
local function etype(val)
   local maintype = type(val)
   if maintype == "table" then
      local ext_type = getmetatable(val).__type
      if ext_type then return ext_type end
   end

   return maintype
end


-- Run a test function, print result.
function M.run(testfun, testname, verbose)
   verbose = verbose or false
   if type(testfun) ~= "function" then 
      error("Must be called with a function.", 2) 
   end
   
   local status, result = pcall(testfun)

   if status then                   -- passed
      if verbose then print("PASS\t" .. testname) else print(".") end
   else                             -- failed
      if etype(res) == "Failure" then --
         if verbose then print("FAIL", tostring(res)) else print("F") end
--             local descr = 
--             print("FAIL", descr)
--          else
--             print("F")
      else
         if verbose then print("ERROR", res) else print("!") end 
      end
   end
end


-- Randomized testing, a la QuickCheck.
-- ( (random) -> 'a) -> ( 'a -> bool ) -> bool
function M.random_test(gen_fun, prop_fun, seed, count)
   if seed then math.randomseed(seed) end
   count = count or 100

   for i=1,count do
      local randval = gen_fun()
      assert_true(prop_fun(randval))
   end
end



-- Failure objects
F = { __index = F, __type = "Failure" }

function F.__tostring(f)
   local e_and_g
   if f.expected and f.got then
      e_and_g = string.format(" (expected %s, got %s)", 
                              f.expected, f.got)
   end
   return string.format([[%s(%d): %s%s]], 
                        f.testname or "", 
                        f.line or -1, 
                        f.description or "",
                        e_and_g or "")
end

function M.Failure(descr, line, expected, got)
   local fail = { description = descr, line = line }
   if expected then fail.expected = expected end
   if got then fail.got = got end
   return setmetatable(fail, F)
end


--------------------------
-- Some test conditions --
--------------------------

-- Basics
function judge(condit, descr, expected, got)
   if condit then 
      return    -- passed
   else
      local line = nil --TODO: get from call stack
      error(M.Failure(descr, line, expected, got), 3)
   end
end

function M.assert_true(val, msg)    return judge(val, msg or "failed") end
function M.always_pass(msg)    return judge(true, msg) end
function M.always_fail(msg)    return judge(false, msg) end
function M.assert_eq(val, expected)
   return judge(val == expected, "==", expected, val) 
end
function M.assert_neq(val, expected)
   return judge(val ~= expected, "~=", expected, val) 
end
function M.assert_lt(val, expected)    
   judge(val < expected, "<", expected, val)
end
function M.assert_lte(val, expected)   
   judge(val <= expected, "<=", expected, val)
end
function M.assert_gt(val, expected)    
   judge(val > expected, ">", expected, val)
end
function M.assert_gte(val, expected)   
   judge(val >= expected, ">=", expected, val)
end
function M.assert_type(val, exp_type)  
   judge(type(val) == exp_type, "type", expected, val)
end

function M.assert_contains(container, elt) 
   if type(container) == "string" and type(elt) == "string" then
      judge(string.find(container, elt), "contains", elt, container)
   elseif type(container) == "table" then
      if container[elt] then 
         return true
      else
         for k,v in ipairs(container) do
            if v == elt then return true end
         end
      end
      always_fail("not found")
   end
end


-- If the module has a __test suite function in its metatable, run that,
-- otherwise scan through the exported functions from a module 
-- for those beginning with prefix, and run them as tests.
-- TODO: setup and teardown
function M.runtests(pkgname, verbose, prefix)
   prefix = prefix or "test_"

   -- Run package test suite, if defined.
   local pkg = require(pkgname)
   local tests = getmetatable(pkg).__tests
   if type(tests) == "table" then
      for i, t in pairs(tests) do
         t:doit(verbose)
      end

      return
   end

   -- Else, run every function beginning with prefix (e.g. "test_") as test.
   for name, val in ipairs(pkg) do
      if (string.find(name, prefix) == 1
       and type(val) == "function") then
         M.run(val, name, verbose)
      end
   end
end


-------------------------
-- Test case object -- --
-------------------------

local T = { __index = T, __type = "Test" }

local function run_hook(hook)
   if type(hook) == "function" then hook() end
end

function T:run(verbose)
   run_hook(self.setup)

   run(self:doit(), self.name, verbose)

   run_hook(self.teardown)
end

function T:doit()
   always_fail("forgot to define the test")
end

-- constructor
function M.Test(name, testfun, setup, teardown)
   local test = { name = name}

   if testfun then test.doit = testfun end
   if setup then test.setup = setup end
   if teardown then test.teardown = teardown end

   return setmetatable({}, T)
end


return M
