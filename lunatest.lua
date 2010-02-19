------------------------------------------------------------------------
--
-- Copyright (c) 2009 Scott Vokes <scott@silentbicycle.com>
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.
--
------------------------------------------------------------------------
--
-- This is a library for randomized testing with Lua.
-- For usage and examples, see README and the test suite.
--
------------------------------------------------------------------------

------------
-- Module --
------------

-- standard libraries used
local debug, io, math, os, string, table =
   debug, io, math, os, string, table

-- required core global functions
local assert, error, ipairs, pairs, pcall, print, setmetatable, tonumber =
   assert, error, ipairs, pairs, pcall, print, setmetatable, tonumber
local fmt, tostring, type, unpack = string.format, tostring, type, unpack
local getmetatable, setmetatable, xpcall = getmetatable, setmetatable, xpcall

-- Get containing env, Lua 5.1 and 5.2-compatible.
local getenv = getfenv or debug.getfenv

---Use lhf's random, if available. It provides an RNG with better
-- statistical properties, and it gives consistent values across OSs.
-- http://www.tecgraf.puc-rio.br/~lhf/ftp/lua/#lrandom
pcall(require, "random")
local random = random

---If available, use luasocket's gettime() for timestamps.
pcall(require, "socket")
local now = socket and socket.gettime

-- Get env immediately wrapping module, to put assert_ tests there.
local _importing_env = getenv()
local dump = my.dump


-- #####################
-- # Utility functions #
-- #####################

local function printf(...) print(string.format(...)) end

local function result_table(name)
   return { name=name, pass={}, fail={}, skip={}, err={} }
end

local function combine_results(to, from)
   local s_name = from.name
   for _,set in ipairs{"pass", "fail", "skip", "err" } do
      local fs, ts = from[set], to[set]
      for name,val in pairs(fs) do
         ts[s_name .. "." .. name] = val
      end
   end
end

local function is_func(v) return type(v) == "function" end

local function count(t)
   local ct = 0
   for _ in pairs(t) do ct = ct + 1 end
   return ct
end


-- ###########
-- # Results #
-- ###########

local RPass = {}
local passMT = {__index=RPass}
function RPass:tostring_char() return "." end
function RPass:add(s, name) s.pass[name] = self end
function RPass:type() return "pass" end
function RPass:tostring(name)
   return fmt("PASS: %s()%s", name, self.template or "")
end


local RFail = {}
local failMT = {__index=RFail}
function RFail:tostring_char() return "F" end
function RFail:add(s, name) s.fail[name] = self end
function RFail:type() return "fail" end
function RFail:tostring(name)
   return fmt("FAIL: %s(): " .. (self.template or "Expected %q, got %q%s"),
              name, tostring(self.exp or ""),
              tostring(self.got or ""),
              self.msg and (" - " .. self.msg) or "")
end


local RSkip = {}
local skipMT = {__index=RSkip}
function RSkip:tostring_char() return "s" end
function RSkip:add(s, name) s.skip[name] = self end
function RSkip:type() return "skip" end
function RSkip:tostring(name)
   return fmt("SKIP: %s()", name)
end


local RError = {}
local errorMT = {__index=RError}
function RError:tostring_char() return "E" end
function RError:add(s, name) s.err[name] = self end
function RError:type() return "error" end
function RError:tostring(name)
   return self.msg or
      fmt("ERROR (in %s(), couldn't get traceback)", name)
end


local function genRes(mt, t)
   return setmetatable(t, mt)
end 


local function Pass(t) return setmetatable(t or {}, passMT) end
local function Fail(t) return setmetatable(t, failMT) end
local function Skip(t) return setmetatable(t, skipMT) end
local function Error(t) return setmetatable(t, errorMT) end


-- ##############
-- # Assertions #
-- ##############

---Renamed standard assert.
old_assert = assert
local checked = 0

local function wraptest(flag, t)
   template = template or "Expected %q, got %q%s"
   checked = checked + 1
   if not flag then
      error(Fail(t))
   end
end

function fail(msg) error(Fail { msg=msg, template="(Failed)" }) end
function skip(msg) error(Skip { msg=msg }) end


---got == true.
function assert(got, msg)
   wraptest(got, { exp=true, got=got, msg=msg,
                   template="Expected success." })
end

assert_true = assert

---got == false.
function assert_false(got, msg)
   wraptest(not got, { exp="false", got=got, msg=msg,
                       template="Expected %s, got %s%s" })
end

--got == nil
function assert_nil(got, msg)
   wraptest(got == nil, { exp="nil", got=got, msg=msg })
end

--got ~= nil
function assert_not_nil(got, msg)
   wraptest(got ~= nil, { exp="nil", got=got, msg=msg })
end

---exp == got.
function assert_equal(exp, got, msg)
   wraptest(exp == got, { exp=exp, got=got, msg=msg })
end

---exp ~= got.
function assert_not_equal(exp, got, msg)
   wraptest(exp ~= got, { exp=exp, got=got, msg=msg })
end

---val > lim.
function assert_gt(lim, val, msg)
   wraptest(val > lim, { exp=lim, got=val, msg=msg })
end

---val >= lim.
function assert_gte(lim, val, msg)
   wraptest(val >= lim, { exp=lim, got=val, msg=msg })
end

---val < lim.
function assert_lt(lim, val, msg)
   wraptest(val < lim, { exp=lim, got=val, msg=msg })
end

---val <= lim.
function assert_lte(lim, val, msg)
   wraptest(val <= lim, { exp=lim, got=val, msg=msg })
end

---#val == len.
function assert_len(len, val, msg)
   wraptest(#val == len, { exp=lim, got=val, msg=msg })
end

---#val ~= len.
function assert_not_len(len, val, msg)
   wraptest(#val ~= len, { exp=lim, got=val, msg=msg })
end

---Test that the string s matches the pattern exp.
function assert_match(exp, s, msg)
   wraptest(exp:match(s), { exp=exp, got=s, msg=msg,
                              template="Expected pattern to match" })
end

---Test that the string s doesn't match the pattern exp.
function assert_not_match(exp, s, msg)
   wraptest(not exp:match(s),
            { exp=exp, got=s, msg=msg,
              template="Expected pattern to not match" })
end

---Test that val is a boolean.
function assert_boolean(val, msg)
   wraptest(type(val) == "boolean",
            { exp="boolean", got=type(val), msg=ms,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a boolean.
function assert_not_boolean(val, msg)
   wraptest(type(val) ~= "boolean",
            { exp="boolean", got=type(val), msg=ms,
              template="Expected type other than %s but got type %s%s" })
end

---Test that val is a number.
function assert_number(val, msg)
   wraptest(type(val) == "number",
            { exp="number", got=type(val), msg=msg,
              template"Expected type of %s but got type %s%s" })
end

---Test that val is not a number.
function assert_not_number(val, msg)
   wraptest(type(val) ~= "number",
            { exp="number", got=type(val), msg=msg,
              template"Expected type other than %s but got type %s%s" })
end

---Test that val is a string.
function assert_string(val, msg)
   wraptest(type(val) == "string",
            { exp="string", got=type(val), msg=msg,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a string.
function assert_not_string(val, msg)
   wraptest(type(val) ~= "string",
            { exp="string", got=type(val), msg=msg,
              template="Expected type other than %s but got type %s%s" })
end

---Test that val is a table.
function assert_table(val, msg)
   wraptest(type(val) == "table",
            { exp="table", got=type(val), msg=msg,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a table.
function assert_not_table(val, msg)
   wraptest(type(val) ~= "table",
            { exp="table", got=type(val), msg=msg,
              template="Expected type other than %s but got type %s%s" })
end

---Test that val is a function.
function assert_function(val, msg)
   wraptest(type(val) == "function",
            { exp="function", got=type(val), msg=msg,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a function.
function assert_function(val, msg)
   wraptest(type(val) ~= "function",
            { exp="function", got=type(val), msg=msg,
              template="Expected type other than %s but got type %s%s" })
end

---Test that val is a thread (coroutine).
function assert_thread(val, msg)
   wraptest(type(val) == "thread",
            { exp="thread", got=type(val), msg=msg,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a thread (coroutine).
function assert_not_thread(val, msg)
   wraptest(type(val) ~= "thread",
            { exp="thread", got=type(val), msg=msg,
              template="Expected type other than %s but got type %s%s" })
end

---Test that val is a userdata (light or heavy).
function assert_userdata(val, msg)
   wraptest(type(val) == "userdata",
            { exp="userdata", got=type(val), msg=msg,
              template="Expected type of %s but got type %s%s" })
end

---Test that val is not a userdata (light or heavy).
function assert_not_userdata(val, msg)
   wraptest(type(val) ~= "userdata",
            { exp="userdata", got=type(val), msg=msg,
              template="Expected type other than %s but got type %s%s" })
end

---Test that a value has the expected metatable.
function assert_metatable(exp, val, msg)
   local mt = getmetatable(val)
   wraptest(mt == exp,
            { exp=exp, got=mt, msg=msg,
              template="Expected metatable %s but got %s%s" })
end

---Test that a value does not have a given metatable.
function assert_not_metatable(exp, val, msg)
   local mt = getmetatable(val)
   wraptest(mt ~= exp,
            { exp=exp, got=mt, msg=msg,
              template="Expected metatable other than %s but got %s%s" })
end

---Test that the function raises an error when called.
function assert_error(f, msg)
   local ok, err = pcall(f)
   wraptest(not ok,
            { exp="an error", got=ok or err, msg=msg,
              template="Expected an error" })
end


---Run a test case with randomly instantiated arguments,
-- running the test function f opt.count (default: 100) times.
-- @param opt A table with options, or just a test name string.<br>
--    opt.count: how many random trials to perform<br>
--    opt.seed: Start the batch of trials with a specific seed<br>
--    opt.always: Always test these seeds (for regressions)<br>
--    opt.show_progress: Whether to print a . after every opt.tick trials.<br>
--    opt.seed_limit: Max seed to allow.<br>
--    opt.max_failures, max_errors, max_skips: Give up after X of each.<br>
-- @param f A test function, run as f(unpack(randomized_args(...)))
-- @param ... the arg specification. For each argument, creates a
--    random instance of that type.<br>
--    boolean: return true or false<br>
--    number n: returns 0 <= x < n, or -n <= x < n if negative.
--              If n has a decimal component, so will the result.<br>
--    string: Specifiedd as "(len[,maxlen]) (pattern)".<br>
--        "10 %l" means 10 random lowercase letters.<br>
--        "10,30 [aeiou]" means between 10-30 vowels.<br>
--    function: Just call (as f()) and return result.<br>
--    table or userdata: Call v.__random() and return result.<br>
-- @usage 
function assert_random(opt, f, ...)
   -- Stub. Exported to the same namespace, but code appears below.
end


-- ####################
-- # Module beginning #
-- ####################

---Unit testing module, with extensions for random testing.
module("lunatest")


-- #########
-- # Hooks #
-- #########

local iow = io.write

local function print_totals(r)
   local ps, fs = count(r.pass), count(r.fail)
   local ss, es = count(r.skip), count(r.err)
   local buf = {"\n---------- Tests finished, ",
                "with %d check(s) ----------\n",
                "  %d passed, %d failed, ",
                "%d error(s), %d skipped."}
   printf(table.concat(buf), checked, ps, fs, es, ss)
end


---Default behavior.
default_hooks = {
   begin = false,
   begin_suite = function(s_env, tests)
                    iow(fmt("-- Starting suite %q, %d test(s)\n  ",
                            s_env.name, count(tests)))
                 end,
   end_suite = false,
   pre_test = false,
   post_test = function(name, res) iow(res:tostring_char()) end,
   done = function(r)
             print_totals(r)
             for _,ts in ipairs{ r.fail, r.err } do
                for name,res in pairs(ts) do
                   print(res:tostring(name))
                end
             end
          end
}


---Default verbose behavior.
verbose_hooks = {
   begin = function(res, suites)
              local s_ct = count(suites)
              if s_ct > 0 then
                 printf("Starting tests, %d suite(s)", s_ct)
              end
           end,
   begin_suite = function(s_env, tests)
                    printf("-- Starting suite %q, %d test(s)",
                           s_env.name, count(tests))
                 end,
   end_suite =
      function(s_env)
         local ps, fs = count(s_env.pass), count(s_env.fail)
         local ss, es = count(s_env.skip), count(s_env.err)
         printf("  * Finished suite %q, +%d -%d E%d s%d",
                s_env.name, ps, fs, es, ss)
      end,
   pre_test = false,
   post_test = function(name, res)
                  printf(res:tostring(name))
               end,
   done = function(r) print_totals(r) end
}

setmetatable(verbose_hooks, {__index = default_hooks })


-- ################
-- # Registration #
-- ################

local suites = { }

---Check if a function name should be considered a test key.
function is_test_key(k)
   return type(k) == "string" and k:match("_*test.*")
end

local function get_tests(mod)
   local ts = {}
   for k,v in pairs(mod) do
      if is_test_key(k) and type(v) == "function" then
         ts[k] = v
      end
   end
   return ts
end


---Add a file as a test suite.
-- @param modname The module to load as a suite. The file is
-- interpreted in the same manner as require "modname". All
-- functions whose names begin or end with "test" (optionally
-- preceded by _s) will be added as test cases.
function suite(modname)
   local ok, err = pcall(
      function()
         local mod = require(modname)
         suites[modname] = get_tests(s)
      end)
   if not ok then
      print("Error loading test suite: " .. modname)
   end
end


-- ###########
-- # Running #
-- ###########

local ok_types = { pass=true, fail=true, skip=true }

local function err_handler(name)
   return function (e)
             if e.type and ok_types[e.type()] then return e end
             local msg = fmt("ERROR in %s():\n\t%s", name, tostring(e))
             msg = debug.traceback(msg, 3)
             return Error { msg=msg }
          end
end


local function run_test(name, test, suite, hooks)
   local result
   if is_func(hooks.pre_test) then hooks.pre_test(name) end
   local t_pre, t_post          --timestamps. requires luasocket.
   if now then t_pre = now() end
   if is_func(suite.setup) then suite.setup(name) end
   
   local ok, err = xpcall(test, err_handler(name))
   if ok then err = Pass() end
   result = err

   -- TODO: log tests w/ no assertions?
   result:add(suite, name)

   if is_func(suite.teardown) then suite.teardown(name) end
   if now then t_post = now() end
   if t_pre and t_post then result.elapsed = t_post - t_pre end
   if is_func(hooks.post_test) then hooks.post_test(name, result) end
end


---Run all known test suites, with given configuration hooks.
-- @param hooks Override the default hooks.
-- @param just Only run a specific suite. TODO
function run(hooks, just)
   -- also check the namespace it's run in
   hooks = hooks or {}
   setmetatable(hooks, {__index = default_hooks})

   local results = result_table("main")

   local env = getenv(3)
   if env then suites.main = get_tests(env) end

   if hooks.begin then hooks.begin(results, suites) end

   for sname,tests in pairs(suites) do
      local t_ct = count(tests)
      if t_ct > 0 then
         local suite = result_table(sname)
         if hooks.begin_suite then hooks.begin_suite(suite, tests) end
         suite.tests = suite
         for name, test in pairs(tests) do
            run_test(name, test, suite, hooks)
         end
         if hooks.end_suite then hooks.end_suite(suite) end
         combine_results(results, suite)
      end
   end
   if hooks.done then hooks.done(results) end
   return results
end


-- ########################
-- # Randomization basics #
-- ########################

local _r
if random then
   _r = random.new()
end

---Set random seed.
function set_seed(s) _r:seed(s) end

---Get a random value low <= x <= high.
function random_int(low, high)
   return _r:value(low, high)
end

---Get a random bool.
function random_bool() return random_int(0, 1) == 1 end

---Get a random float low <= x < high.
function random_float(low, high)
   return get_int(low, high) + _r:value()
end


if not random then
   set_seed = math.randomseed
   random_bool = function() return math.random(0, 1) == 1 end
   random_int = function(l, h) return math.random(l, h) end
   random_float = function(l, h)
                     return random_int(l, h - 1) + math.random()
                  end
end

-- Lua_number's bits of precision. IEEE 754 doubles have 52.
local function determine_accuracy()
   for i=1,128 do
      if 2^i == (2^i + 1) then return i - 1 end
   end
   return 128   --long long ints?
end
local bits_of_accuracy = determine_accuracy()


-- ##################
-- # Random strings #
-- ##################


-- Return a (() -> random char) iterator from a pattern.
local function parse_pattern(pattern)
   local cs = {}                --charset
   local idx = 1
   local len = string.len(pattern)
   assert(len > 0, "Cannot generate pattern from empty string.")

   local function at_either_end() return #cs == 0 or #cs == len end
   local function slice(i) return string.sub(pattern, i, i) end

   while idx <= len do
      local c = slice(idx)

      if c == "-" then
         if at_either_end() then
            cs[#cs+1] = c    --literal - at start or end
         else                --range
            local low = string.byte(slice(idx-1)) + 1
            local high = string.byte(slice(idx+1))
            assert(low < high, "Invalid character range: " .. pattern)
            for asc=low,high do
               cs[#cs+1] = string.char(asc)
            end
            idx = idx + 1
         end

      elseif c == "%" then
         local nextc = slice(idx + 1)
         cs[#cs+1] = charclass("%" .. nextc)
         idx = idx + 1

      else
         cs[#cs+1] = c
      end
      idx = idx + 1
   end

   cs = table.concat(cs)
   local len = string.len(cs)
   assert(len > 0, "Empty charset")

   return function()
             local idx = random_int(0, len) + 1
             return string.sub(cs, idx, idx)
          end
end


-- Read a random string spec, return a config table.
local function parse_randstring(s)
   local low, high, rest = string.match(s, "([0-9]+),?([0-9]*) (.*)")
   if low then                  --any match
      if high == "" then high = low end
      return { low = tonumber(low),
               high = tonumber(high),
               gen = parse_pattern(rest) }
   else
      local err = "Invalid random string spec: " .. s
      error(err, 2)
   end
end


-- Generate a random string.
-- @usage e.g. "20 listoftwentycharstogenerate" or "10,20 %l".
function random_string(spec)
   local info = parse_randstring(spec)
   local ct, diff
   diff = info.high - info.low
   if diff == 0 then ct = info.low else
      ct = random_int(diff + 2) + info.low
   end
   
   local acc = {}
   for i=1,ct do
      acc[i] = info.gen(self)
   end
   return table.concat(acc)
end


-- #########################
-- # General random values #
-- #########################

-- Generate a random number, according to arg.
local function gen_number(arg)
   arg = arg or math.huge
   local signed = (arg < 0)
   local float
   if signed then float = (math.ceil(arg) ~= arg) else
      float = (math.floor(arg) ~= arg)
   end

   local f = float and random_float or random_int
   if signed then
      return f(arg, -arg)
   else
      return f(arg)
   end
end


-- Create an arbitrary instance of a value.
local function generate_arbitrary(arg)
   local t = type(arg)
   if t == "number" then
      return gen_number(arg)
   elseif t == "function" then
      return arg(gen_number())        -- assume f(number) -> val
   elseif t == "string" then
      return random_string(arg)
   elseif t == "table"  or t == "userdata" then
      assert(arg.__random, t .. " has no __random method")
      -- assume arg.__random(number) -> val
      return arg.__random(gen_number())
   elseif t == "boolean" then
      return random_bool()
   else
      error("Cannot randomly generate values of type " .. t .. ".")
   end
end


-- Process test case args.
local function proc_args(arglist)
   local name, pred, args
   if type(arglist[1]) == "string" then
      name = arglist[1]
      table.remove(arglist, 1)
   else name = "" end

   local pred = arglist[1]
   assert(type(pred) == "function",
          "First argument (after optional name) must be trial function.")
   table.remove(arglist, 1)

   return name, pred, arglist
end


local random_test_defaults = {
   count = 100,
   max_failures = 10,
   max_errors = 5,
   max_skips = 50,
   random_bound = 2^bits_of_accuracy,
   seed_limit = math.min(1e13, 2^bits_of_accuracy),
   always = {},
   seed = nil,
   show_progress = false,
   tick = 10,
}

local function seed_digits(t)
   return math.floor(math.log(t.seed_limit) / math.log(10)) + 1
end


local function random_args(args)
   local as = {}
   for i=1,#args do
      as[i] = generate_arbitrary(args[i])
   end
   return as
end


local function new_seed(limit)
   limit = limit or 1e13
   return random_int(0, limit)
end


local function get_seeds(t)
   local ss = {}
   for _,r in ipairs(t) do
      if r.seed then ss[#ss+1] = r.seed end
   end
   return ss
end


local function run_randtest(seed, f, args, r, limit)
   local try_ct = 0
   while r.tried[seed] and try_ct < 50 do
      seed = new_seed(limit)
      try_ct = try_ct + 1
   end
   if try_ct >= 50 then
      error(Fail { template = "Exhausted all seeds" })
   end
   set_seed(seed)
   r.tried[seed] = true
   local r_args = random_args(args)
   local ok, err = pcall(function() f(unpack(r_args)) end)
   if ok then
      err = Pass()
      err.seed = seed
      r.ps[#r.ps+1] = err
   else
      result = err
      result.seed = seed
      local rt = result:type()
      if rt == "pass" then r.ps[#r.ps+1] = err
      elseif rt == "fail" then r.fs[#r.fs+1] = err
      elseif rt == "error" then r.es[#r.es+1] = err
      elseif rt == "skip" then r.ss[#r.ss+1] = err
      else error("unmatched")
      end
   end
   
   seed = new_seed(limit)
   r.ts = r.ts + 1
   return seed
end


local function assert_random(opt, f, ...)
   local args = { ... }
   if type(opt) == "string" then
      opt = { name=opt }
   elseif type(opt) == "function" then
      table.insert(args, 1, f)
      f = opt
      opt = {}
   end
      
   setmetatable(opt, { __index=random_test_defaults })

   local seed = opt.seed or os.time()

   local r = { ps={}, fs={}, es={}, ss={}, ts=0, tried={} }

   -- Run these seeds every time, for easy regression testing.
   for _,s in ipairs(opt.always) do
      run_randtest(s, f, args, r, opt.seed_limit)
   end

   for i=1,opt.count do
      seed = run_randtest(seed, f, args, r, opt.seed_limit)
      if #r.ss > opt.max_skips or
         #r.fs > opt.max_failures or
         #r.es > opt.max_errors then break
      end
      if opt.show_progress and i % opt.tick == 0 then iow(".") end
   end
   
   if #r.es > 0 then
      local seeds = get_seeds(r.es)
      error(Fail { template = fmt("%d tests, %d error(s).\n   %s",
                                  r.ts, #r.es,
                                  table.concat(seeds, "\n   ")),
                   seeds = seeds})
   elseif #r.fs > 0 then
      local seeds = get_seeds(r.fs)
      error(Fail { template = fmt("%d tests, %d failure(s).\n   %s",
                                  r.ts, #r.fs,
                                  table.concat(seeds, "\n   ")),
                   seeds = seeds})
   elseif #r.ss >= opt.max_skips then
      error(Fail { template = fmt("Too many cases skipped.")})
   else
      error(Pass { template = fmt(": %d cases passed.", #r.ps) })
   end
end


-- Put it in the same namespace as the other assert_ functions.
_importing_env.assert_random = assert_random
