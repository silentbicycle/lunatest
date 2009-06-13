require "moonunit"

local function label(l) print("\n----- " .. l ) end

-- Initialize verbose tester.
local t = moonunit.new{ verbose=true, count=10 }

label("Verbose, case name")
t:test("not_div_by_3", function(x) return x % 3 ~= 0 end, 37)

label("Verbose, no case name")
t:test(function(x) return x % 3 ~= 0 end, 37)


-- New tester, more cases, reduce verbosity to "only on error".
t = moonunit.new{ count=10000, verbose="error_only" }

label("Only show errors, enough cases that showing progress matters")
t:test("flipcoin", function(x) return x end, 
       false)        -- bool -> return random bool

label("Same, no label")
t:test(function(x) return x end, 
       true)


-- Show progress, show arguments on error and failure (default).
-- Seeds can be made arbitrarily small; if all possible seeds have 
-- been used, it will terminate the test and print the results.
t = moonunit.new{ count=1000 }

-- String patterns
label("Test with string pattern")
t:test("has_vowels", 
       function(s) return string.match(s, "[aeiou]") end,
       "50 %l")        -- generate 50 random lowercase letters
t:test("has_an_x",
       function(s) return string.match(s, "x") end,
       "100,150 %l")       -- ususally passes, but not always...
t:test("has_no_Xs", 
       function(s) return not string.match(s, "X") end,
       "500 %l")       --lowercase only, won't find "X"s

label("(Error on 'xx', skip on '  '.)")
t:test("show_error", 
       function(s) 
          if string.match(s, "  ") then 
             error("skip")      -- discard this run and retry
          elseif string.match(s, "xx") then
             error("(a very fake crash)")
          end
          return string.match(s, ".") 
       end,
       "4,6 %l ")


-- Ints and floats
t = moonunit.new{ count=1000, verbose="error_only" }

label("Int / float tests")
t:test("is_pos", function(n) return n >= 0 end, 100)
t:test("is_pos2", function(n) return (n - 1) >= 0 end, 100)
t:test("rounds_down", 
       function(n) return math.fmod(n, 1) < .2 end, 
       -10.1)          -- return a signed float -10 <= x < 10


-- Finally, with an actual generator function
label("From a function")
t:test("table_length", function(t) return #t < 6 end,
       function(r)
          local t = {}
          -- For every "heads", add another val
          while r:get_bool() do t[#t+1] = "(v)" end
          return t
       end)


label("Same, with metatable")
local flipMT = { __random= function(r)
                              local t = {}
                              while r:get_bool() do t[#t+1] = "(v)" end
                              return t
                           end }
local fliptable = setmetatable( {}, { __index = flipMT })
t:test("table_length2", function(t) return #t < 6 end, fliptable)


label "Now, let's test the actual program..."

-- This caught a bug in itself. :)
label("Test that the RNG wrapper matches expected bounds")
t = moonunit.new{ count=1000 }

local low, high
for run, pair in ipairs{ {1, 2}, {2, 10}, {-1, 1}, 
                         {-100, 100}, {-1, 0}, {0, 1} } do
   low, high = pair[1], pair[2]
   if high > 1 then             -- can't generate randints 0 <= x < 1
      t:test("int_genL_" .. run,
             function(i) return i < high end,
             function(r) return r:get_int(high) end)
   end   
   t:test("int_genLH_" .. run, function(i) 
                                      return i < high and i >= low 
                                   end,
          function(r) return r:get_int(low, high) end)
   
   if high > 1 then
      t:test("float_genL_" .. run, function(t) return t < high end, 
             function(r) return r:get_float(high) end)
   end
   
   t:test("float_genLH_" .. run, function(t) return 
                                        t < high and t >= low 
                                     end, 
          function(r) return r:get_float(low, high) end)
end

label("Test string lengths are generated correctly")
for run,high in ipairs{ 11, 12, 13 } do
   t:test("strlenL_" .. high, 
          function(s) return string.len(s) >= 10 end,
          "10," .. high .. " %l")

   local used = {}  -- also test overall generation

   t:test("strlenLH_" .. high,
          function(s) 
             local len = string.len(s)
             used[len] = true
             return len >= 10 and len <= high
          end,
          "10," .. high .. " %l")

   for i=10,high do 
      if not used[i] then 
         print("Never made any strings of length " .. i)
      end
   end
end
