require "random" -- NB. Use at least version X, not what luarocks has!

-----------
-- Usage --
-----------





printf = function(...) io.stdout:write(string.format(...)) end

-- FIXME
local randbound = 2 ^ 30        --upper bound for random.valuei()



--------------------
-- Random strings --
--------------------

-- "" -> ?
-- "NUM[charset]" generate NUM chars from charset
-- "LOW<HI[charset]" generate between LOW and HI
--               chars from charset
--
-- A charset can either be [chars], regexp style, or
-- one of the %d %w %s etc. sigils.
-- Create a random string.
function gen_string(twister, arg)
   local spec = assert(parse_randstring(arg), "bad")
   local ct = twister:valuei(spec.hi - spec.low) + spec.low

   print(ct)
   local acc = {}
   for i=1,ct do
      acc[#i] = spec.gen(twister)
   end
   return table.concat(acc)
end


-- Read a random string spec, return a config table.
function parse_randstring(s)
   local low, hi, rest = string.match(s, "([0-9]+)<([0-9]+)(.*)")
   if low then
      return { low = tonumber(low),
               hi = tonumber(hi),
               gen = parse_s(rest) }
   else
      ct, rest = string.match(s, "([0-9])(.*)")
      if ct then
         ct = tonumber(ct)
         return { low = ct, hi = ct, gen = parse_s(rest) }
      end
   end

   local err = "Invalid random string spec: " .. s
   error(err, 2)
end


-- Return a (twister -> random char) iterator of a str spec.
function parse_str(s)
   local charset = string.match(s, "%[(.*)%]")
   -- TODO: ranges, and see LRM pg. 77
   local len
   if charset then
      len = string.len(charset)
   else
      error("TODO")
   end
   return function(twister)
             local idx = twister:valuei(len) - 1
             return string.sub(charset, idx, idx)
          end
end


-----------------
-- Other types --
-----------------

-- Random bool. Simple.
function gen_boolean(twister)
   return twister:valuei(1) == 1
end


-- Generate a random number, according to arg.
function gen_number(twister, arg)
   local signed = (arg < 0)
   local float
   if signed then float = (math.ceil(arg) ~= arg) else
      float = (math.floor(arg) ~= arg)
   end

   local res = twister:valuei(arg) - 1
   if signed and twister:valuei(2) == 2 then res = -res end
   if float then res = res + twister:value() end
   return res
end


-- Create an arbitrary instance of a value.
function generate(twister, arg)
   if type(arg) == "number" then
      return gen_number(twister, arg)
   elseif type(arg) == "function" then
      -- assume f(twister) -> val
      return arg(twister)
   elseif type(arg) == "string" then
      return gen_string(twister, arg)
   elseif type(arg) == "userdata" then
      local mt = getmetatable(arg)
      return mt.__random(twister, arg)
   elseif type(arg) == "table" then
      local mt = getmetatable(arg)
      if mt and mt.__random then return mt.__random(twister, arg) else
         error("for tables, use generator or __random in metatable")
      end
   elseif type(arg) == "boolean" then
      return gen_boolean(twister)
   else
      error("Cannot randomly generate values of type " .. type(arg) .. ".")
   end
end


-- Process args.
function proc_args(arglist)
   local name, pred, args
   if type(arglist[1]) == "string" then
      name = arglist[1] .. ": "
      table.remove(arglist, 1)
   else name = "" end

   local pred = arglist[1]
   assert(type(pred) == "function",
          "First argument (after optional name) must be trial function.")
   table.remove(arglist, 1)

   return name, pred, arglist
end


-- Construct a random tester.
function new(t)
   t = t or {}
   local count = t.count or 100
   local seed = t.seed
   local skips_allowed = t.skips or 50
   local verbose = t.verbose or false
   local progress = t.progress or 10
   local twister = random.new()

   local function test(...)
      local name, check, args = proc_args{ ... }

      seed = seed or os.time()
      printf("%s%d trials, seed %s: ", name, count, seed)
      twister:seed(seed)

      local passed = 0
      local failed = 0
      local skipped = 0
      local errors = 0

      local thisseed = twister:valuei(randbound)
      for trial=1,count do
         local rand = twister:clone()
         local callargs = {}
         for i=1, #args do
            callargs[i] = generate(twister, args[i])
         end
         local status, result = pcall(check, unpack(callargs))

         if status then
            if result == true or result == "pass" then passed = passed + 1
            elseif result == "skip" then skipped = skipped + 1
            else failed = failed + 1 end
         elseif result == "skip" then    -- "exception"-style, error("skip")
            skipped = skipped + 1
            if skipped > skips_allowed then
               printf("\n%sWarning -- %d skips at %d of %d trials (+%d, -%d).\n",
                      name, skips_allowed, trial, count, passed, failed)
               return
            end
         elseif result == "fail" then failed = failed + 1    --same, error("fail")
         else                                                --actual error
            printf("\nERROR: seed %d, %s", thisseed, result or "(nil)")
            errors = errors + 1
         end

         if trial % progress == 0 and count > 0 then
            printf(".")
            io.flush(io.stdout)
         end
         thisseed = twister:valuei(randbound)
      end
      printf(" (+%d, -%d, s%d, e%d)\n", passed, failed, skipped, errors)
   end

   return test
end
