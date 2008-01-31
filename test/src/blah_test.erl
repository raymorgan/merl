-module (blah_test).

-include_lib ("eunit/include/eunit.hrl").
  
my_test_() -> [
    {"should ensure 2 is 2",
      ?_assertMatch(2, 2)
    },
    
    {"should error...",
      ?_assertMatch(no_match, bad())
    }
  ].
  
  
bad() -> merl.router:match([{add, {"/:p"}}], "/path", get).