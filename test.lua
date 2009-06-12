require "moonunit"

local function label(l) print("\n----- " .. l ) end


-- Initialize verbose tester.
local tester = moonunit.new{ verbose=true, count=10 }

label("Verbose, case name")
tester("not_div_by_3", function(x) return x % 3 ~= 0 end, 37)

label("Verbose, no case name")
tester(function(x) return x % 3 ~= 0 end, 37)

-- New tester, more cases, reduce verbosity to "only on error".
tester = moonunit.new{ count=10000, progress=500, verbose="error_only" }

label("Only show errors, enough cases that showing progress matters")
tester("flipcoin", function(x) return x end, 
       false)        -- bool -> return random bool

label("Same, no label")
tester(function(x) return x end, 
       true)


-- Show progress, show arguments on error and failure (default).
tester = moonunit.new{ count=1000, progress=100 }

-- String patterns
label("Test with string pattern")
tester("has_vowels", 
       function(s) return string.match(s, "[aeiou]") end,
       "50 %l")        -- generate 50 random lowercase letters
tester("has_an_x", 
       function(s) return string.match(s, "x") end,
       "200 %l")
tester("has_no_Xs", 
       function(s) return not string.match(s, "X") end,
       "200 %l")                --lowercase only

label("(Error on 'xx', skip on '  '.)")
tester("show_error", 
       function(s) 
          if string.match(s, "  ") then 
             error("skip")
          elseif string.match(s, "xx") then
             error("(a very fake crash)")
          end
          return string.match(s, ".") 
       end,
       "4 %l ")


-- Ints and floats
tester = moonunit.new{ count=1000, progress=100, verbose="error_only" }

label("Int / float tests")
tester("is_pos", function(n) return n >= 0 end, 100)
tester("is_pos2", function(n) return (n - 1) >= 0 end, 100)
tester("rounds_down", 
       function(n) return math.fmod(n, 1) < .2 end, 
       -10.1)          -- return a signed float -10 <= x < 10
