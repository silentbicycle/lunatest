-----------
-- Usage --
-----------

-- TODO


module(..., package.seeall)


-------------------
-- RNG Interface --
-------------------

local Random = {}          -- prototype for RNG interface
-- get_bool()           -> get a random bool
-- get_float(low, high) -> get a random float 0 <= x < high
-- get_int(bound)       -> get a random int 0 <= x < bound
-- get_int(low, high)   -> get a random int low <= x < high
-- get_string(spec)     -> get a random string, according to spec
-- set_seed(s)          -> set the new random seed to s


-- Lua_number's bits of precision. IEEE 754 doubles have 52.
local function determine_accuracy()
   for i=1,128 do
      if 2^i == (2^i + 1) then return i - 1 end
   end
   return 128   --long long ints?
end
local bits_of_accuracy = determine_accuracy()


if pcall(function()
            require "sucka"
         end) then
   -- if present, prefer lhf's random module.
   Random.limit = 2^bits_of_accuracy
   print("TODO")
else
   -- Fall back on math.random
   Random.limit = math.min(2^30, 2^bits_of_accuracy)
   Random.set_seed = function(r, s) 
                        math.randomseed(s) 
                     end
   Random.get_int = function(r, low, hi) 
                       if hi then 
                          return math.random(low, hi) - 1
                       else
                          return math.random(low) - 1
                       end
                    end
   Random.get_float = function(r, low, hi)
                         return math.random(low, hi) - 1 + math.random()
                      end
end


function Random:tostring() return "(RNG interface)" end



function Random.new()
   return setmetatable({}, { __index=Random, __tostring=Random.tostring })
end


--------------------
-- Random strings --
--------------------

-- For valid char classes, see Lua Reference Manual 5.1, p. 77
-- or http://www.lua.org/manual/5.1/manual.html#5.4.1 .
local function charclass(pat)
   local m = {}

   local match, char = string.match, string.char
   for i=0,255 do
      local c = char(i)
      if match(c, pat) then m[#m+1] = c end
   end

   return table.concat(m)
end


-- Return a (Random -> random char) iterator from a pattern.
local function parse_pattern(s)
   local cs = {}                --charset
   local pattern = string.match(s, ".+")
   -- TODO: ranges, and see LRM pg. 77
   
   if pattern then
      local idx = 1
      local len = string.len(pattern)
      local function at_either_end() return #cs == 0 or #cs == len end
      local function slice(i) return string.sub(pattern, i, i) end
      while idx <= len do
         local c = slice(idx)
         if c == "-" then
            if at_either_end() then
               cs[#cs+1] = c    --literal - at start or end
            else                --range
--                print(idx, table.concat(cs), pattern)
               local low = string.byte(slice(idx-1)) + 1
               local high = string.byte(slice(idx+1))
--                print(low, high, string.char(low), string.char(high))
               assert(low < high, "Invalid character range: " .. pattern)
               for asc=low,high do
--                   print(low, high, string.char(asc))
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
   else
      local err = "Pattern not found in: " .. s
      error(err, 2)
   end

   cs = table.concat(cs)
   local len = string.len(cs)
   assert(len > 0, "Empty charset")
--    print(len, pattern, cs)
   return function(r)
             local idx = r:get_int(len) + 1
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


-- Use with arg e.g. "20 listoftwentycharstogenerate" or "10,20 %l".
function Random:get_string(arg)
   local spec = assert(parse_randstring(arg), "bad pattern")
   local ct, diff
   diff = spec.high - spec.low
   if diff == 0 then ct = spec.low  else
      ct = self:get_int(diff) + spec.low
   end

   local acc = {}
   for i=1,ct do
      acc[i] = spec.gen(self)
   end
   return table.concat(acc)
end


-----------------
-- Other types --
-----------------

-- Random bool. Simple.
function Random:get_bool()
   return self:get_int(2) == 1
end


-- Generate a random number, according to arg.
local function gen_number(r, arg)
   local signed = (arg < 0)
   local float
   if signed then float = (math.ceil(arg) ~= arg) else
      float = (math.floor(arg) ~= arg)
   end

   local f = float and r.get_float or r.get_int
   if signed then
      return f(r, arg, -arg)
   else
      return f(r, arg)
   end
end


-- Create an arbitrary instance of a value.
local function generate(r, arg)
   local t = type(arg)
   if t == "number" then
      return gen_number(r, arg)
   elseif t == "function" then
      return arg(r)                   -- assume f(r) -> val
   elseif t == "string" then
      return r:get_string(arg)
   elseif t == "table"  or t == "userdata" then
      assert(arg.__random, t .. " has no __random method")
      return arg:__random(r)          -- assume arg:__random(r) -> val
   elseif t == "boolean" then
      return r:get_bool()
   else
      error("Cannot randomly generate values of type " .. t .. ".")
   end
end


-- Process args.
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


-- Construct a random testing function.
function new(t)
   t = t or {}
   local count = t.count or 100
   local seed = t.seed
   local skips_allowed = t.skips or 50
   local verbose = t.verbose or false
   local progress = t.progress or 10
   local randy = Random.new()
   local randbound = 2^bits_of_accuracy
   local out = t.log or io.stdout

   local function log(...) out:write(string.format(...)) end

   local function progress_hook(res, seed, t, p, f, s, e)
      if verbose == true then
         log("%-4s %-20s (+%d, -%d, s%d, e%d)\n", 
             res, seed, p, f, s, e)
      elseif (t % progress == 0 and count > 0) then
         log "."    -- "Brevity is the soul of wit." - W. S.
         io.flush(out)
      end
   end

   local function test(...)
      local name, check, args = proc_args{ ... }

      seed = seed or os.time()
      local padded_name = (name ~= "" and name .. ": ") or ""
      log("%s%d trials, seed %s: ", padded_name, count, seed)
      if verbose == true then log("\n") end
      randy:set_seed(seed)

      local passed = 0
      local failed = 0
      local skipped = 0
      local errors = 0

      for trial=1,count do
         -- Get + save the current seed, so we can report it.
         local thisseed = randy:get_int(randy.limit)
         randy:set_seed(thisseed)
         
         local callargs = {}
         for i=1, #args do
            callargs[i] = generate(randy, args[i])
         end
         local status, res = pcall(check, unpack(callargs))
         
         if status then         -- completed without error(...)
            -- results of "pass", "skip" caught implicitly
            if res then res = "pass" else res = "fail" end
         else   -- actual error or error("skip"), error("fail"), etc.
            local ok = { pass=true, skip=true, fail=true }
            local errval = res
            res = string.sub(res, -4) -- code preceded by filename:line
            if not ok[res] then
               res = "error"
               log("\nERROR: seed %d, %s", thisseed, 
                   res or "(error() returned nil)")
               errors = errors + 1
            end
         end
         
         if res == "pass" then passed = passed + 1
         elseif res == "fail" then
            if verbose ~= "error_only" then
               log("\n%sFailed -- %s (+%d, -%d)", 
                  padded_name, thisseed, passed, failed)
            end
            failed = failed + 1
         elseif res == "skip" then 
            skipped = skipped + 1
            if skipped > skips_allowed then
               log("\n%sWarning -- %d skips at %d of %d trials (+%d, -%d).\n",
                  padded_name, skips_allowed, trial, count, passed, failed)
               return
            end
         else 
            assert(res == "error")
            errors = errors + 1
         end

         if (res == "fail" and verbose ~= "error_only") 
         or res == "error" then
            log("\n")
            for idx,arg in ipairs(callargs) do 
               log("    %d -- %s\n", idx, tostring(arg))
            end
         end

         progress_hook(res, thisseed, trial, passed, failed, skipped, errors)
      end
      if verbose == true then log("total %s", name) end
      local overall_status = (passed == count and "PASS" or "FAIL")
      log(" %s (+%d, -%d, s%d, e%d)\n", 
          overall_status, passed, failed, skipped, errors)
      if verbose == true then log("\n") end

      if overall_status == "PASS" then return true end
   end

   return test
end
